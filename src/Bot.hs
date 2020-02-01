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
import           Data.Coerce
-- My modules
import           Types
import qualified Twitch.Lib                           as Twitch

nick :: String
nick = "otter_chaos_repair"

pass :: String
pass = "oauth:4o5ilipoc2piphhh04m7x653r296f8"

runBot
  :: String -- ^ channel to join
  -> TChan DispatcherMsg -- ^ You need to update this
  -> IO ()
runBot chan tchan = mask $ \restore -> do
  putStrLn $ "Twitch bot: started"
  client <- Twitch.connect
  putStrLn $ "Twitch bot: connected"
  Twitch.authenticate client nick pass
  putStrLn $ "Twitch bot: authenticated"
  Twitch.joinChannel client chan
  putStrLn $ "Twitch bot joined channel: " <> chan
  putStrLn $ "Twitch bot processing messages..."
  restore (Twitch.processMessages client tchan) `catch` \(e :: SomeException) ->
    putStrLn $ "Bot listening " <> chan <> " has stoped because " <> show e
