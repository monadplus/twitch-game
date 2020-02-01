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
module API.Types where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Map.Strict          (Map)
import           Data.Map.Strict          as Map
import           Data.Text                (Text)
import           Data.UUID.Types          (UUID)
import           GHC.Generics             (Generic)
import           JSON.Internals
import qualified Servant

---------------------------------------------

type App = ReaderT ServerState Servant.Handler

---------------------------------------------

newtype Channel = Channel { _unChannel :: Text }
  deriving (Show, Generic)
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

newtype SessionID = SessionID { _unSessionID :: UUID }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype PlayerName = PlayerName { _unPlayerName :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Command = Command { _unCommand :: Text }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Color = Color { _unColor :: Text } -- e.g. "#0000FF"
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
  { _id    :: PlayerName
  , _color :: Color
  } deriving (Show, Generic)
    deriving (ToJSON, FromJSON) via (NoPrefix PlayerResp)

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
-- Dispatcher

data DispatcherMsg =
    Start Channel SessionID
  | Stop SessionID
  | TwitchMsg
        String -- ^ Channel
        String -- ^ User
        Text {-raw commands-}
        (Maybe (Map String String)) -- PrivateMessageTags
  deriving Show

----------------------------------------------------

data PlayerStats = PlayerStats
  { _lastCommand :: Maybe Command
  , _playerColor :: Color
  }

type NewPlayers = Map PlayerName Color
type AllPlayers = Map PlayerName PlayerStats
type Task = Async ()

data ServerState = ServerState
  { _activeGames :: TVar (Map Channel (SessionID, [Command]))
  , _runningBots :: TVar (Map SessionID Task)
  , _newPlayers  :: TVar NewPlayers
  , _allPlayers  :: TVar AllPlayers
  , _dQueue      :: TChan DispatcherMsg
  }

makeLenses ''Channel
makeLenses ''SessionID
makeLenses ''PlayerName
makeLenses ''Command
makeLenses ''PlayerStats
makeLenses ''StartResp
makeLenses ''StartReq
makeLenses ''PlayerResp
makeLenses ''CommandResp
makeLenses ''GameUpdate
makeLenses ''ServerState

newServerState :: IO (ServerState)
newServerState = do
  games      <- newTVarIO Map.empty
  bots       <- newTVarIO Map.empty
  newPlayers <- newTVarIO Map.empty
  allPlayers <- newTVarIO Map.empty
  queue      <- newTChanIO
  return $ ServerState games bots newPlayers allPlayers queue
