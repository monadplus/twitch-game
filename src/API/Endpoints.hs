{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module API.Endpoints where

------------------------------------------------------------

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Reader
import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Map.Strict                      (Map)
import           Data.Map.Strict                      as Map
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.UUID.Types                      (UUID)
import qualified Data.UUID.Types                      as UUID
import           GHC.Generics                         (Generic)
import           Network.Wai                          (Application)
import qualified Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.RequestLogger
import           Servant
import           Servant.Server                       as Servant
import           System.Random                        (randomIO)
import           Text.Parsec                          ((<?>))
import qualified Text.Parsec                          as Parsec

-- My Modules
import           API.Errors
import           API.Types

------------------------------------------------------------

-- TODO STM and IO doesn't fit well.
startGame :: StartReq -> App StartResp
startGame (StartReq chan cmds) = do
  tvar  <- view activeGames
  games <- unSTM $ readTVar tvar
  when (Map.member chan games) $ throwError gameAlreadyStarted
  sessionID <- liftIO $ fmap SessionID randomIO
  liftIO $ putStrLn $ "Starting game with " <> show sessionID
  tchan <- view dQueue
  unSTM $ writeTChan tchan (Start chan sessionID)
  unSTM $ writeTVar tvar (Map.insert chan (sessionID, cmds) games)
  return (StartResp sessionID)

------------------------------------------------------

-- | Sends the collected commands to the server flushing the current ones.
updateGame :: UUID -> App GameUpdate
updateGame uuid = do
  let sessionID = SessionID uuid
  liftIO $ putStrLn ("Updating game: " <> show sessionID)
  ctx <- view Prelude.id
  games <- unSTM $ readTVar (ctx ^. activeGames)
  let optCmds = preview ( folded
                        . _2
                        . filtered (elemOf _1 sessionID)
                        ) $ Map.toList games
  case optCmds of
    Just (_, cmds) -> do
      new <- unSTM $ ctx ^. newPlayers . to (flip swapTVar Map.empty)
      all <- unSTM $ ctx ^. allPlayers . to readTVar
      return $ toGameUpdate cmds new all
    Nothing -> throwError sessionNotFound

toGameUpdate
  :: [Command]
  -> NewPlayers
  -> AllPlayers
  -> GameUpdate
toGameUpdate cmds new all = do
  GameUpdate newPlayers newCmds
    where
      commandMap = Map.fromList (zip cmds [1..])

      commandToCode c = commandMap ! c

      newPlayers = fmap (\(name, color) -> PlayerResp name color) $ Map.toList new

      newCmds = concat $
        keys all <&> \name ->
          let (PlayerStats optCmd color) = all ! name
           in optCmd & maybe []
                 (\cmd -> [CommandResp name (commandToCode cmd)])

------------------------------------------------------

-- | No-op if the session does not exist
endGame :: UUID -> App NoContent
endGame uuid = do
  let sessionID = SessionID uuid
  liftIO $ putStrLn ("Deleting game: " <> show uuid)
  tchan <- view dQueue
  unSTM $ writeTChan tchan (Stop sessionID)
  return NoContent

------------------

unSTM :: (MonadIO m) => STM a -> m a
unSTM = liftIO . atomically
