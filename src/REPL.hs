module REPL(shell) where
import System.Posix.Process
import System.Posix.Types
import System.Directory
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Text.Parsec hiding (token)
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative ((<$>), (<*))
import Data.String.Utils


data Action     = Read | Write | Append deriving (Show, Eq)
data Joint      = Pipe | And deriving (Show, Eq)
data Cmd        = SimpleCmd String [String] |
                  RedirectCmd Cmd Action String |
                  CompoundCmd Cmd Joint Cmd |
                  SubShell Cmd deriving (Show, Eq)
newtype History = History {history :: [Cmd]}

callSystem :: String -> [String] -> IO ()
callSystem "cd" args = setCurrentDirectory $ head args

builtinCommands :: [String]
builtinCommands = ["cd"]

repl :: History -> IO ()
repl history = do
  dir  <- getCurrentDirectory
  putStr $ dir ++ "$ "
  cmd <- getCmd
  line <- getLine
  case line of
    "exit"       -> return ()
    otherwise    -> do
      if elem cmd builtinCommands then callSystem cmd args
      else do
        -- Fork the child
        childPID <- exec cmd args []
        wait childPID
        return ()
      repl $ extend (SimpleCmd cmd args) history
      where tokens = words line
            cmd    = head tokens
            args   = tail tokens

shell :: IO ()
shell = repl newHistory

extend :: Cmd -> History -> History
extend cmd hist = History $ cmd:cmds
  where cmds = history hist

newHistory :: History
newHistory = History []


exec :: String -> [String] -> [(String, String)] -> IO ProcessID
exec cmd args env = forkProcess $ executeFile cmd True args $ Just env

-- Shamelessly stolen from https://github.com/dblarons/haskell-shell/blob/master/Main.hs
-- |Wait on a command that does not have file descriptors
wait :: ProcessID -> IO ProcessStatus
wait childPID =
    do status <- getProcessStatus True False childPID
       case status of
         Nothing -> fail "Error: Nothing from getProcessStatus"
         Just ps -> return ps

-- IO

data CmdString = PartialCmd String | CompleteCmd String


getCommandString :: String -> String -> [Char] -> CmdString
getCommandString original ""             []          = CompleteCmd original
getCommandString original ""             _           = PartialCmd original
getCommandString original "\\"           []          = PartialCmd $ init original
getCommandString original "\""           []          = PartialCmd original
getCommandString original "'"            []          = PartialCmd original
getCommandString original ('\\':'\"':xs) qts         = getCommandString original xs qts
getCommandString original ('\\':'\'':xs) qts         = getCommandString original xs qts
getCommandString original ('"':xs)       ('"':rest)  = getCommandString original xs rest
getCommandString original ('"':xs)       qts         = getCommandString original xs ('"':qts)
getCommandString original ('\'':xs)      ('\'':rest) = getCommandString original xs rest
getCommandString original ('\'':xs)      qts         = getCommandString original xs ('\'':qts)
getCommandString original (x:xs)         qts         = getCommandString original xs qts


processLine :: String -> IO Cmd
processLine prefix = do
  let s = if null prefix then "" else "> "
  putStr s
  line <- getLine
  let fullLine = prefix ++ line
  case getCommandString fullLine fullLine [] of
    CompleteCmd cmd -> return $ parseCmd cmd
    PartialCmd  cmd -> processLine cmd

getCmd :: IO Cmd
getCmd = processLine ""

-- parser

toJoint :: String -> Joint
toJoint "|"  = Pipe
toJoint "&&" = And


toAction :: String -> Action
toAction "<"  = Read
toAction ">"  = Write
toAction ">>" = Append

token :: Parser String
token = do
  spaces
  haskellString <|> many1 alphaNum
  where haskellString = stringLiteral $ makeTokenParser haskellDef

jointParser :: Parser Joint
jointParser = do
  spaces
  link <- pipe <|> andThen
  return $ toJoint link
  where pipe       = string "|"
        andThen    = string "&&"

actionParser :: Parser (Action, String)
actionParser = do
  spaces
  link <- read <|> write <|> append
  spaces
  file <- token
  let action = toAction link
  return (action, file)
  where read   = string "<"
        write  = string ">"
        append = string ">>"



simpleCmdParser :: Parser Cmd
simpleCmdParser = do
  command  <- token
  args     <- many token
  let cmd = SimpleCmd command args
  action   <- optionMaybe actionParser
  let res = case action of
              Nothing             -> cmd
              Just (redirect, file) -> RedirectCmd cmd redirect file
  return res

compoundCmdParser :: Parser Cmd
compoundCmdParser = do
  fst  <- simpleCmdParser
  link <- jointParser
  snd  <- compoundCmdParser
  return $ CompoundCmd fst link snd

cmdParser :: Parser Cmd
cmdParser = simpleCmdParser <|> compoundCmdParser

subShellParser :: Parser Cmd
subShellParser = do
  spaces
  char '('
  cmd <- cmdParser
  spaces
  char ')'
  return $ SubShell cmd


entryParser :: Parser Cmd
entryParser = subShellParser <|> cmdParser

parseCmd :: String -> Cmd
parseCmd input = case parse entryParser "(unknown)" input of
                   Right cmd -> cmd
                   Left  err -> error $ show err
