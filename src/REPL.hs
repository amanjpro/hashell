import System.Posix.Process
import System.Posix.Types
import System.Directory
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative ((<$>), (<*))


data Joint      = Pipe | Read | Write | Append
data Cmd        = Cmd {cmd :: String, args :: [String]} |
                  ComplexCmd {cmd1 :: Cmd, joint :: Joint, cmd2 :: Cmd}
newtype History = History {history :: [Cmd]}

callSystem :: String -> [String] -> IO ()
callSystem "cd" args = setCurrentDirectory $ head args

builtinCommands :: [String]
builtinCommands = ["cd"]

repl :: History -> IO ()
repl history = do
  dir  <- getCurrentDirectory
  putStr $ dir ++ "$ "
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
      repl $ extend (Cmd cmd args) history
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


-- parser
token :: Parser String
token = do
  spaces
  haskellString <|> many1 alphaNum
  where haskellString = stringLiteral $ makeTokenParser haskellDef

jointParser :: Parser String
jointParser = pipe <|> read <|> write <|> append
  where pipe   = string "|"
        read   = string "<"
        write  = string ">"
        append = string ">>"

cmdParser :: Parser Cmd
cmdParser = do
  cmd  <- token
  args <- many token
  return $ Cmd cmd args

cmdsParser :: Parser Cmd
cmdsParser = do
  cmds <- cmd <|> many $ cmd <$> joint <$> cmd
  return Cmd "" []

