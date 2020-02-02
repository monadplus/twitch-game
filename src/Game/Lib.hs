{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Game.Lib
  ( runGame
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Map.Strict                      (Map)
import           Data.Map.Strict                      as Map
import           Data.Monoid
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as Text (hGetLine, hPutStrLn, putStr)
import           GHC.Generics                         (Generic)
import           JSON.Internals
import           Network.Wai                          (Application)
import qualified Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.RequestLogger
import           Servant
import           Servant.Server
import           System.IO
import           Text.Parsec                          ((<?>))
import qualified Text.Parsec                          as Parsec
import           Data.Coerce

-- My modules
import           API.Types
import qualified Twitch.Lib                           as Twitch
import           API.CommandParser

--------------------------------------------------------------------

runGame :: ServerState -> IO ()
runGame ctx = forever $ do
  let q = ctx ^. dQueue
  msg <- atomically $ readTChan q
  case msg of

    Start chan sessionID ->
      processStart chan sessionID

    Stop sessionID ->
      processStop sessionID

    KeepAlive sessionID ->
      keepAlive sessionID

    TwitchMsg rawChannel user msg' tags ->
      processTwitchMsg rawChannel user msg' tags

  where

    processStart :: Channel -> SessionID -> IO ()
    processStart chan sessionID = do
      putStrLn $ "Starting bot for session: " <> show sessionID
      task <- async $ runTwitchBot (chan ^. unChannel . to T.unpack)
                                   (ctx ^. dQueue)
      timeoutTask <- cancelIn10Seconds (ctx ^. dQueue) sessionID
      atomically $ modifyTVar (ctx ^. runningBots)
                              (Map.insert sessionID (task, timeoutTask))

    --------

    processStop :: SessionID -> IO ()
    processStop sessionID = do
      putStrLn $ "Stopping bot for session: " <> show sessionID
      taskOpt <- atomically $ do
             games <- readTVar $ ctx ^. activeGames
             let channelOpt = ipreview (ifolded . filtered (elemOf _1 sessionID)) games
             case channelOpt of
               Just (channel', _) -> do
                 -- Remove active game
                 modifyTVar (ctx ^. activeGames) (Map.delete channel')
                 -- Remove players from the server
                 gamePlayers' <- readTVar $ ctx ^. gamePlayers
                 let playersToDelete = (Map.toList gamePlayers') ^.. folded . filtered ((== channel'). snd) . _1
                 Prelude.foldr (\playerName acc -> do
                                   acc
                                   let deletePlayer from' = modifyTVar (ctx ^. from') (Map.delete playerName)
                                   deletePlayer newPlayers
                                   deletePlayer allPlayers
                                   deletePlayer gamePlayers
                               ) (pure ()) playersToDelete
               Nothing ->
                 return ()
             -- Stop running bot
             view ( runningBots
                  . to (flip stateTVar $ \bots ->
                          (,) (bots !? sessionID)
                              (Map.delete sessionID bots)
                       )
                  ) ctx

      case taskOpt of
        Just (task, timeout) -> do
          cancel timeout
          cancel task
        Nothing ->
          return ()

    --------

    keepAlive :: SessionID -> IO ()
    keepAlive sessionID = do
      putStrLn "Keep Alive"
      let tvar = ctx ^. runningBots
      bots <- atomically $ readTVar tvar
      case bots ^. at sessionID of
        Just (task, timeout) -> do
          -- Cancel current timeout and add a new one.
          cancel timeout
          newTimeout <- cancelIn10Seconds (ctx ^. dQueue) sessionID
          atomically $ writeTVar tvar (bots & at sessionID ?~ (task, newTimeout))
        Nothing ->
          return ()

    --------

    processTwitchMsg
      :: String  -- ^ Channel
      -> String  -- ^ User
      -> Text    -- ^ Raw Command
      -> Maybe (Map String String)
      -> IO ()
    processTwitchMsg rawChan user rawCommand tagsOpt = do
      games <- atomically $ ctx ^. activeGames . to readTVar
      let validCommands =  games ^?! ix channel' . _2
      case parseCommand validCommands rawCommand of

        Left _ -> do
          Text.putStr $ "Couldn't parse the message: " <> rawCommand
          return () -- Usually not an error.

        Right Join -> do
          putStrLn "Join command parsed!"
          players <- atomically $ ctx ^. allPlayers . to readTVar
          let color' =
                maybe (Color "#000000")
                      (Color . T.pack) $ do
                        tags <- tagsOpt
                        tags !? "color"
          case players !? playerName of
            Just _ ->
              return () -- Already exist
            Nothing -> do
              atomically $ do
                ctx ^. newPlayers . to (flip modifyTVar (Map.insert playerName color'))
                ctx ^. allPlayers . to (flip modifyTVar (Map.insert playerName (PlayerStats Nothing color')))
                ctx ^. gamePlayers . to (flip modifyTVar (Map.insert playerName channel'))

        Right (OtterCommand command')-> do
          putStr $ "OtterCommand parsed: " <> show command'
          void $ atomically $ do
            players <- readTVar (ctx ^. allPlayers)
            writeTVar (ctx ^. allPlayers) (players & ix playerName . lastCommand .~ (Just command'))

      where
        channel' = Channel (T.pack rawChan)
        playerName = PlayerName (T.pack user)


cancelIn10Seconds :: TChan DispatcherMsg -> SessionID -> IO TimeoutTask
cancelIn10Seconds tchan sessionID =
  async $ do
    threadDelay (10 * (10^(6::Int)))
    putStrLn "Timeout!"
    atomically $ writeTChan tchan (Stop sessionID)


----------------------------------

nick :: String
nick = "otter_chaos_repair"

pass :: String
pass = "oauth:4o5ilipoc2piphhh04m7x653r296f8"

runTwitchBot
  :: String -- ^ channel to join
  -> TChan DispatcherMsg -- ^ You need to update this
  -> IO ()
runTwitchBot chan tchan = mask $ \restore -> do
  putStrLn $ "Twitch bot: started"
  client <- Twitch.connect
  putStrLn $ "Twitch bot: connected"
  Twitch.authenticate client nick pass
  putStrLn $ "Twitch bot: authenticated"
  Twitch.joinChannel client chan
  putStrLn $ "Twitch bot joined channel: " <> chan
  putStrLn $ "Twitch bot processing messages..."
  restore (Twitch.processMessages client tchan) `catch` \(_ :: AsyncCancelled) ->
    putStrLn $ "Bot has stopped gracefully."
