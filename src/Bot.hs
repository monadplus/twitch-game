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
module Bot
  ( runBot
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
import qualified Data.Text.IO                         as Text (hGetLine, hPutStrLn)
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
import           Types
-- My modules
import qualified Twitch.Lib                           as Twitch

nick :: String
nick = "otter_chaos_repair"

pass :: String
pass = "oauth:4o5ilipoc2piphhh04m7x653r296f8"

channel :: String
channel = "otter_chaos_repair"

runBot :: ServerState -> IO ()
runBot _ctx = do
  putStrLn $ "Twitch bot: started"
  hSetBuffering stdout NoBuffering
  client <- Twitch.connect
  putStrLn $ "Twitch bot: connected"
  Twitch.authenticate client nick pass
  putStrLn $ "Twitch bot: authenticated"
  Twitch.joinChannel client channel
  putStrLn $ "Twitch bot joined channel: " <> channel
  putStrLn $ "Twitch bot processing messages..."
  concurrently_
    (Twitch.processMessages client stdout)
    (readCommandLine client channel)

readCommandLine :: Twitch.Client -> Twitch.Channel -> IO ()
readCommandLine client channel = forever $ do
  content <- Text.hGetLine stdin
  Twitch.sendMessage client channel content
