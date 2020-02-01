{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module API.Errors (
    gameAlreadyStarted
  , sessionNotFound
  ) where

-----------------------------

import Data.Text (Text)
import Data.String
import Servant.Server
import qualified Data.Aeson as Aeson
import GHC.Generics

-----------------------------

gameAlreadyStarted :: ServerError
gameAlreadyStarted = err200With "The channel is already playing the game."

sessionNotFound :: ServerError
sessionNotFound = err200With "Session ID not found."

-----------------------------
-- Private

newtype ErrorCode = ErrorCode { error_code :: Text }
  deriving (Generic)
  deriving (Aeson.ToJSON)

-- | Unity...
err200 :: ServerError
err200 = ServerError { errHTTPCode = 200
                     , errReasonPhrase = "Bad Request"
                     , errBody = ""
                     , errHeaders = []
                     }

err200With :: Text -> ServerError
err200With body =
  let err = ErrorCode body
   in err200 { errBody = (Aeson.encode err) }


