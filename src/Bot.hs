{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Bot
  ( runBot
  ) where

import           Control.Monad
import           Control.Lens
import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           GHC.Generics                         (Generic)
import           Network.Wai                          (Application)
import qualified Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.RequestLogger
import           Servant
import           Servant.Server
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Text.Parsec               ((<?>))
import qualified Text.Parsec               as Parsec
import           Data.Map.Strict           (Map)
import           Data.Map.Strict           as Map
import           JSON.Internals
import           Types

runBot :: ServerState -> IO ()
runBot _ctx = do
  putStrLn ("Twitch Bot started")
  forever $ do
    threadDelay 10000000
    return ()
