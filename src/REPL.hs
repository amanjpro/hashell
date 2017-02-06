module REPL(shell) where

-- Posix modules
import System.Posix.Process
import System.Posix.Types
import System.Posix.IO
import System.Posix.Files

-- System modules
import System.Exit
import System.Directory
import System.IO

-- Parser modules
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Control.Applicative ((<$>), (<*))

-- Util modules
import Data.String.Utils
import Control.Exception.Base (catch)

-- Concurrency
import Control.Concurrent
import Control.Concurrent.MVar

type CloseFds = MVar [Fd]


data Action     = Read | Write | Append deriving (Show, Eq)
data Joint      = Pipe | And deriving (Show, Eq)
data Cmd        = SimpleCmd String [String] |
                  RedirectCmd Cmd Action String |
                  CompoundCmd Cmd Joint Cmd |
                  SubShell Cmd deriving (Show, Eq)
newtype History = History {history :: [Cmd]}

callSystem :: String -> [String] -> IO ()
callSystem "exit" []    = exitSuccess
callSystem "cd"   [arg] = setCurrentDirectory arg

builtinCommands :: [String]
builtinCommands = ["exit", "cd"]

repl :: History -> IO ()
repl history = do
  dir  <- getCurrentDirectory
  putStr $ dir ++ "$ "
  cmd <- getCmd
  closefds <- newMVar []
  res <- runCommand cmd closefds ""
  putStr res
  repl $ extend cmd history

shell :: IO ()
shell = repl newHistory

extend :: Cmd -> History -> History
extend cmd hist = History $ cmd:cmds
  where cmds = history hist

newHistory :: History
newHistory = History []


runCommand :: Cmd -> CloseFds -> String -> IO String
runCommand (SimpleCmd cmd args) closefds input          =
  if cmd `elem` builtinCommands then
    do callSystem cmd args
       return ""
  else do
    (stdinread, stdinwrite) <- createPipe
    (stdoutread, stdoutwrite) <- createPipe
    addCloseFDs closefds [stdinwrite, stdoutread]
    childId <- withMVar closefds (\fds -> forkProcess (runChild cmd args fds stdinread stdoutwrite))
    closeFd stdinread
    closeFd stdoutwrite
    stdinhdl <- fdToHandle stdinwrite
    forkIO $ do hPutStr stdinhdl input
                hClose stdinhdl
    stdouthdl <- fdToHandle stdoutread
    wait childId
    handle <- fdToHandle stdoutread
    hGetContents handle
runCommand (RedirectCmd cmd Read file) closefds input   = do
  content <- readFile file
  runCommand cmd closefds content
runCommand (RedirectCmd cmd Write file) closefds input  = do
  res <- runCommand cmd closefds input
  writeFile file res
  return ""
runCommand (RedirectCmd cmd Append file) closefds input = do
  res <- runCommand cmd closefds input
  appendFile file res
  return ""
runCommand (CompoundCmd cmd1 And cmd2) closefds input   = do
  runCommand cmd1 closefds input
  runCommand cmd2 closefds ""
runCommand (CompoundCmd cmd1 Pipe cmd2) closefds input = do
  res <- runCommand cmd1 closefds input
  runCommand cmd2 closefds res

runChild :: String -> [String] -> [Fd] -> Fd -> Fd -> IO ()
runChild cmd args closefds input output = do
  dupTo input stdInput
  dupTo output stdOutput
  closeFd input
  closeFd output
  let _ = closeFd `map` closefds
  executeFile cmd True args Nothing

-- Add FDs to the list of FDs that must be closed post-fork in a child
addCloseFDs :: CloseFds -> [Fd] -> IO ()
addCloseFDs closefds newfds =
    modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)

-- Remove FDs from the list
removeCloseFDs :: CloseFds -> [Fd] -> IO ()
removeCloseFDs closefds removethem =
    modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist removethem)

    where
    procfdlist fdlist [] = fdlist
    procfdlist fdlist (x:xs) = procfdlist (removefd fdlist x) xs

    -- We want to remove only the first occurance ot any given fd
    removefd [] _ = []
    removefd (x:xs) fd
        | fd == x = xs
        | otherwise = x : removefd xs fd


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

legalChar :: Parser Char
legalChar = (oneOf ['.', '/', ':', '-']) <|> digit <|> letter


singleQuote :: Parser String
singleQuote = do
  string "\""
  contents <- many (((:[]) <$> noneOf ['\'']) <|> string "\\'")
  string "\""
  let content = foldl (++) "" contents
  return $ "\"" ++ content ++ "\""

doubleQuote :: Parser String
doubleQuote = do
  string "'"
  contents <- many (((:[]) <$> noneOf ['\'']) <|> string "\\'")
  string "'"
  let content = foldl (++) "" contents
  return $ "'" ++ content ++ "'"


shellString :: Parser String
shellString = singleQuote <|> doubleQuote

tokenParser :: Parser String
tokenParser = do
  spaces
  many1 legalChar <|> shellString

jointParser :: Parser Joint
jointParser = do
  spaces
  link <- pipe <|> andThen
  return $ toJoint link
  where pipe       = string "|"
        andThen    = string "&&"

actionParser :: Parser Cmd
actionParser = do
  command  <- cmdParser
  spaces
  link <- read <|> write <|> append
  spaces
  file <- tokenParser
  let action = toAction link
  return $ RedirectCmd command action file
  where read   = string "<"
        write  = string ">"
        append = string ">>"



simpleCmdParser :: Parser Cmd
simpleCmdParser = do
  command  <- tokenParser
  args     <- many tokenParser
  return $ SimpleCmd command args

compoundCmdParser :: Parser Cmd
compoundCmdParser = do
  fst  <- simpleCmdParser
  link <- jointParser
  snd  <- compoundCmdParser
  return $ CompoundCmd fst link snd

cmdParser :: Parser Cmd
cmdParser = try compoundCmdParser <|> simpleCmdParser

subShellParser :: Parser Cmd
subShellParser = do
  spaces
  char '('
  cmd <- entryParser
  spaces
  char ')'
  return $ SubShell cmd


entryParser :: Parser Cmd
entryParser = do
  res <- subShellParser <|> try actionParser <|> cmdParser
  eof
  return res

parseCmd :: String -> Cmd
parseCmd input = case parse entryParser "(unknown)" input of
                   Right cmd -> cmd
                   Left  err -> error $ show err
