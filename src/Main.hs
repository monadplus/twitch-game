{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

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

-- data Request = Request { pull_request :: PullRequest}
--   deriving (Generic, FromJSON, Show)
--
-- data PullRequest = PullRequest { number :: Integer }
--   deriving (Generic, FromJSON, Show)
--
-- data Response = Response {}
--   deriving (Generic, ToJSON)
--
-- type WebHook =
--      Header "X-Github-Event" Text
--   :> ReqBody '[JSON] Request
--   :> Post '[JSON] ()
--
-- server :: Server WebHook
-- server (Just "pull_request") request = liftIO (print request)
-- server _ _                           = fail "Oops"

type HelloApi = "hello" :> Get '[JSON] Text

hello :: Text
hello = "hello"

server :: Server HelloApi
server = return hello

application :: Application
application = Network.Wai.Middleware.RequestLogger.logStdoutDev $
  serve (Proxy @HelloApi) server

main :: IO ()
main = Network.Wai.Handler.Warp.run 8081 application
