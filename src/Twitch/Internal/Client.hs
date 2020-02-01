module Twitch.Internal.Client (
    Client
  , connect
  , authenticate
  , joinChannel
  , sendMessage
  , sendPong
  , processMessages
  ) where

----------------------------------------------

import           Control.Monad                (forever)
import           Data.Monoid                  (mappend)
import           Data.Text.IO                 (hGetLine, hPutStrLn)
import           Network                      (PortID (PortNumber), connectTo)
import           System.IO                    (BufferMode (NoBuffering), Handle, hPrint, hSetBuffering, stderr, stdout)
import           Text.Printf                  (hPrintf)

import           Twitch.Internal.Constants    as Twitch
import           Twitch.Internal.Parser
import           Twitch.Internal.Types

import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           API.Types

import           Control.Concurrent.STM.TChan

----------------------------------------------

connect :: IO Client
connect = do
  socket <- connectTo Twitch.server (PortNumber (fromIntegral Twitch.port))
  hSetBuffering socket NoBuffering
  return socket

sendCommand :: Client -> String -> String -> IO ()
sendCommand client = hPrintf client "%s %s\r\n"

authenticate :: Client -> String -> String -> IO ()
authenticate client nick pass = do
  sendCommand client "PASS" pass
  sendCommand client "NICK" nick
  sendCommand client "CAP REQ" ":twitch.tv/tags" -- Tags — Adds IRC V3 message tags to several commands, if enabled with the commands capability.

joinChannel :: Client -> String -> IO ()
joinChannel client channel = do
  sendCommand client "JOIN" ("#" ++ channel)

sendMessage :: Client -> String -> T.Text -> IO ()
sendMessage client channel msg = do
  let formatted = "#" ++ channel ++ " :" ++ (T.unpack msg)
  sendCommand client "PRIVMSG" formatted

sendPong :: Client -> T.Text -> IO ()
sendPong client msg = sendMessage client "PONG" msg

processMessages
  :: Client
  -> TChan DispatcherMsg
  -> IO ()
processMessages client tchan = forever $ do
  line <- hGetLine client
  case parseMessage line of
    Right (Ping pong) -> sendPong client pong -- The server must reply to PING to keep the connection alive.
    Right msg@(PrivateMessage channel user content tags) ->
      forward channel user content tags
    Right msg -> hPutStrLn stdout (T.pack $ show msg)
    Left _    -> hPutStrLn stderr ((T.pack "Error parsing:") `mappend` line)

    where
      forward channel user content tags =
        undefined











