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
module Main where

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
import           Bot
import           JSON.Internals
import           Types

type App = ReaderT ServerState Servant.Handler

type Api =  "game" :> ReqBody '[JSON] StartReq  :> Put   '[JSON] StartResp
       :<|> "game" :> Capture "session_id" UUID :> Get    '[JSON] GameUpdate
       :<|> "game" :> Capture "session_id" UUID :> Delete '[JSON] NoContent

-- TODO
startGame :: StartReq -> App StartResp
startGame _ = do
  uuid <- liftIO $ randomIO
  return $ StartResp (SessionID uuid)


-- TODO
updateGame :: UUID -> App GameUpdate
updateGame = undefined

-- TODO
endGame :: UUID -> App NoContent
endGame _ = return NoContent

server :: ServerT Api App
server = startGame :<|> updateGame :<|> endGame

readerServer :: ServerState -> Server Api
readerServer ctx = hoistServer (Proxy @Api) phi server
  where
    phi app = runReaderT app ctx

runServer
  :: Int
  -> ServerState
  -> IO ()
runServer port ctx =
  Network.Wai.Handler.Warp.run port application
    where
      application :: Application
      application = Network.Wai.Middleware.RequestLogger.logStdoutDev $
        serve (Proxy @Api) (readerServer ctx)

main :: IO ()
main = do
  ctx <- newServerState
  let port = 8080
      api  = runServer port ctx
      bot  = runBot ctx
  putStrLn ("Starting server on port " <> show port)
  concurrently_ api bot
