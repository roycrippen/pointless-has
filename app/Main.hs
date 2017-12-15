module Main where

import qualified Network.WebSockets as WS (runServer)
import           SocketServer       (application)

import           Control.Monad      (forever)
import           CoreLibrary
import qualified Data.Map           as M
import           Interpreter
import           Parser
import           PointlessParser
import           Primitives
import           System.Environment (getArgs)
import           System.Exit        (exitSuccess)
import           System.IO

-- main :: IO ()
-- main = do
--     putStrLn "starting websocket server"
--     WS.runServer "127.0.0.1" 9160 application

main :: IO ()
main = do
  args <- getArgs
  case args of
    x:_ -> case x of
      "web" -> do
        putStrLn "Welcome to the Pointless web server\n"
        putStrLn "starting websocket server"
        WS.runServer "127.0.0.1" 9160 application
      _ -> do
        putStrLn "invalid arg, use \"web\" for wesocket server or no arg for repl"
        exitSuccess
    _ -> do
      putStrLn "Welcome to the Pointless repl"
      putStrLn "Enter a valid Pointless expression or :h for help"
      putStrLn "Pointless> "
      let defs = getQuotations coreDefinitions ++ primitives
          lang = Lang (M.fromList defs) [] [] [] ""
      runPointless lang

q1 :: String
q1 = " 10 [1 10 [] from-to .] times "

runQuot :: String -> Lang -> Lang
runQuot s = runQuotation qs
 where (qs, _):_ = parse nakedQuotations s

runPointless :: Lang -> IO ()
runPointless lang = forever $ do
  putStr "Pointless> "
  hFlush stdout
  quoteStr' <- getLine
  -- putStr "quotation before = "
  -- putStrLn quoteStr'
  let quoteStr = replaceStr "\\n" "\n" quoteStr'
  runCommand quoteStr
    where
      runCommand :: String -> IO ()
      runCommand s =
        case take 2 s of
        ":q" -> exitSuccess
        ":h" -> showHelp >> runPointless lang
        ":l" -> do
          lang' <- loadAndRunFile s lang
          runPointless lang' { result = [], errors = [] }
        ":r" -> showHelp >> runPointless lang
        _    -> do
          let lang' = runQuot s lang
          mapM_ putStrLn (errors lang')
          mapM_ putStrLn (result lang')
          runPointless $ lang' { result = [], errors = [] }

loadAndRunFile :: String -> Lang -> IO Lang
loadAndRunFile file lang = do
  let file' = replaceStr ":l " "" file ++ ".pless"
  source' <- readFile file'
  let source = replaceStr  "\\n" "\n" source'
      lang' = runQuot source lang
  mapM_ putStrLn (errors lang')
  mapM_ putStrLn (result lang')
  return lang'

showHelp :: IO ()
showHelp = do
  putStrLn ":h             show help"
  putStrLn ":q             exit"
  putStrLn "Ctrl+c         exit"
  putStrLn ":l <filename>  load a Pointless script"
  putStrLn ":r             reload Pointless script"
  putStrLn "or enter a valid Pointless expression"
  putStrLn "Pointless> "

replaceStr :: String -> String -> String -> String
replaceStr _ _ [] = []
replaceStr old new str = go str
  where
    go [] = []
    go str'@(x:xs) =
      let (prefix, rest) = splitAt n str'
      in
        if old == prefix
        then new ++ go rest
        else x : go xs
    n = length old


