module Twitch.Internal.Parser (
    parseMessage
  , Message (..)
  ) where

----------------------------------

import           Data.ByteString.Char8 (pack)
import           Data.Map              (fromList)
import           Data.Text             (Text)
import           Data.Text.Encoding    (decodeUtf8)
import           Text.Parsec
import           Text.Parsec.Text      (Parser)
-- My modules
import           Twitch.Internal.Constants      as Twitch
import           Twitch.Internal.Types
import           Debug.Trace

----------------------------------

utf8String :: String -> Text
utf8String = decodeUtf8 . pack

rest :: Parser String
rest = manyTill anyChar eof

nickname :: Parser String
nickname = many (alphaNum <|> char '_')

username :: Parser User
username = nickname

channel :: Parser Channel
channel = do
  char '#' >> many (alphaNum <|> char '_')

commandPrefix :: Parser User
commandPrefix = do
  char ':' >> nickname >> char '!'
  user <- username
  char '@' >> nickname >> char '.' >> string Twitch.tmi >> space
  return user

privmsgTag :: Parser (String, String)
privmsgTag = do
  key <- manyTill anyChar (char '=')
  value <- manyTill anyChar (char ';' <|> (lookAhead $ char ' '))
  return (key, value)

privmsgTags :: Parser PrivateMessageTags
privmsgTags = do
  char '@'
  tags <- manyTill (try privmsgTag) (char ' ')
  return $ fromList tags

privateMessage :: Parser Message
privateMessage = do
  mbTags <- optionMaybe privmsgTags
  user <- commandPrefix
  string "PRIVMSG" >> space
  chan <- channel
  space >> char ':'
  content <- rest
  return $ PrivateMessage chan user (utf8String $ init content) mbTags

joinMessage :: Parser Message
joinMessage = do
  user <- commandPrefix
  string "JOIN" >> space
  chan <- channel
  return $ JoinMessage chan user

partMessage :: Parser Message
partMessage = do
  user <- commandPrefix
  string "PART" >> space
  chan <- channel
  return $ JoinMessage chan user

serverMessage :: Parser Message
serverMessage = do
  char ':' >> (string Twitch.tmi <|> (username >> char '.' >> string Twitch.tmi)) >> space -- didn't find a better way...
  code <- many1 digit
  space >> username >> space
  mbChan <- optionMaybe (optional (char '=' >> space) >> channel)
  case mbChan of -- kind of ugly here too
    Just _  -> space >> char ':'
    Nothing -> char ':'
  content <- rest
  return $ ServerMessage mbChan (read code) (utf8String content)

jtvCommand :: Parser Message
jtvCommand = do
  ((try $ string (":jtv!jtv@jtv." ++ Twitch.tmi)) <|> string ":jtv") >> space -- still not great
  string "PRIVMSG" >> space
  username >> space >> char ':'
  cmd <- many1 upper
  space
  content <- rest
  return $ JtvCommand cmd (utf8String content)

jtvMode :: Parser Message
jtvMode = do
  string ":jtv" >> space
  string "MODE" >> space
  chan <- channel
  space
  mode <- string "+o" -- only this mode for now
  space
  user <- username
  return $ JtvMode chan mode user

pingMessage :: Parser Message
pingMessage = do
  string "PING" >> space
  message <- rest
  return $ Ping (utf8String message)

ircMessage :: Parser Message
ircMessage =  try privateMessage
          <|> try joinMessage
          <|> try partMessage
          <|> try serverMessage
          <|> try jtvCommand
          <|> try jtvMode
          <|> pingMessage

parseMessage :: Text -> Either ParseError Message
parseMessage content =
  parse ircMessage "IRC" (traceShowId content)-- content --
