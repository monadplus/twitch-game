{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module Types where

import           Control.Concurrent.STM
import           Control.Lens
import           Data.Aeson
import           Data.Map.Strict        (Map)
import           Data.Map.Strict        as Map
import           Data.Text              (Text)
import           Data.UUID.Types        (UUID)
import           GHC.Generics           (Generic)
import           JSON.Internals

---------------------------------------------

newtype Channel = Channel { _unChannel :: Text }
  deriving (Show, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype SessionID = SessionID { _unSessionID :: UUID }
  deriving (Show, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype PlayerName = PlayerName { _unPlayerName :: Text }
  deriving (Show, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Command = Command { _unCommand :: Text }
  deriving (Show, Generic)
  deriving newtype (FromJSON, ToJSON)

---------------------------------------------


data StartReq = StartReq
  { _channel  :: Channel
  , _commands :: [Command]
  } deriving (Show, Generic)
    deriving (ToJSON, FromJSON) via (NoPrefix StartReq)

data StartResp = StartResp
  { _session_id :: SessionID
  } deriving (Show, Generic)
    deriving (ToJSON, FromJSON) via (NoPrefix StartResp)

---------------------------------------------

data PlayerResp = PlayerResp
  { _id :: PlayerName
  } deriving (Show, Generic)
    deriving (ToJSON, FromJSON) via (NoPrefix PlayerResp)

toPlayerResp :: Player -> PlayerResp
toPlayerResp = undefined

data CommandResp = CommandResp
  { _player_id :: PlayerName
  , _command   :: Int -- ^ The position of the command in the list
  } deriving (Show, Generic)
    deriving (ToJSON, FromJSON) via (NoPrefix CommandResp)

data GameUpdate = GameUpdate
  { _new_players     :: [PlayerResp]
  , _player_commands :: [CommandResp]
  } deriving (Show, Generic)
    deriving (ToJSON, FromJSON) via (NoPrefix GameUpdate)


----------------------------------------------------

data Player = Player
  { _unLastCommand :: Maybe Command
  }

data ServerState = ServerState
  { _activeGames :: TVar (Map Channel SessionID)
  -- ^^^^^^^ end is an explicit message (no timeout for now)
  , _players     :: TVar (Map PlayerName Player)
  -- ^^^^^^^ players may have left.. (TODO we could check that)
  }

makeFields ''Channel
makeFields ''SessionID
makeFields ''PlayerName
makeFields ''Command
makeFields ''Player
makeFields ''StartResp
makeFields ''StartReq
makeFields ''PlayerResp
makeFields ''CommandResp
makeFields ''GameUpdate
makeFields ''ServerState

newServerState :: IO (ServerState)
newServerState = do
  g <- newTVarIO Map.empty
  p <- newTVarIO Map.empty
  return $ ServerState g p
