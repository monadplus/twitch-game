module API.CommandParser (
    GameCommand(..)
  , parseCommand
  ) where

----------------------------------

import           API.Types
import           Control.Lens
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Lens
import           Text.Parsec
import           Text.Parsec.Text      (Parser)

----------------------------------

data GameCommand =
    Join
  | OtterCommand Command

joinCommand :: Parser GameCommand
joinCommand = string "join" *> return Join

otterCommand :: [String] -> Parser GameCommand
otterCommand =
  fmap (OtterCommand . Command . T.pack) .
    foldr (\command acc -> string command <|> acc)
          (unexpected "Not a valid command.")

commandParser :: [String] -> Parser GameCommand
commandParser validCommands =
  char '!' *>
    (try joinCommand <|> otterCommand validCommands)

parseCommand
  :: [Command] -- ^ Valid commands
  -> Text      -- ^ Raw command
  -> Either ParseError GameCommand
parseCommand validCommands =
  let commands = validCommands ^.. traversed . unCommand . unpacked
   in parse (commandParser commands) "Command Parsing"

