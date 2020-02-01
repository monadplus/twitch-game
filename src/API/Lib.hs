{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module API.Lib where

------------------------------------------------------------

import           API.Endpoints
import           API.Types
import           Control.Monad.Reader
import           Data.UUID.Types                      (UUID)
import           Network.Wai                          (Application)
import qualified Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.RequestLogger
import           Servant

------------------------------------------------------------

type Api =  "game" :> ReqBody '[JSON] StartReq  :> Put   '[JSON] StartResp
       :<|> "game" :> Capture "session_id" UUID :> Get    '[JSON] GameUpdate
       :<|> "game" :> Capture "session_id" UUID :> Delete '[JSON] NoContent

server :: ServerT Api App
server = startGame :<|> updateGame :<|> endGame

readerServer :: ServerState -> Server Api
readerServer ctx = hoistServer (Proxy @Api) phi server
  where
    phi app = runReaderT app ctx

application :: ServerState -> Application
application ctx = Network.Wai.Middleware.RequestLogger.logStdoutDev $
  serve (Proxy @Api) (readerServer ctx)

type Port = Int

runServer :: Port -> ServerState -> IO ()
runServer port ctx = Network.Wai.Handler.Warp.run port (application ctx)
