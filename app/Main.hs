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
      putStrLn ":h for help"
      putStrLn ":q to exit (or ctrl+c)"
      putStrLn "or enter a Pointless expression\n"
      let defs = getQuotations coreDefinitions ++ primitives
          lang = Lang (M.fromList defs) [] [] [] ""
      runPointless lang

q1 :: String
q1 = " 10 [1 10 [] from-to .] times "

runQuot :: String -> Lang -> Lang
runQuot s = runQuotation qs
 where (qs, _) = head $ parse nakedQuotations s

runPointless :: Lang -> IO ()
runPointless lang = forever $ do
  putStr "Pointless> "
  hFlush stdout
  quoteStr <- getLine
  case quoteStr of
    ":q" -> exitSuccess
    ":h" -> showHelp >> runPointless lang
    _    -> do
      let lang' = runQuot quoteStr lang
      mapM_ putStrLn (errors lang')
      mapM_ putStrLn (result lang')
      runPointless $ lang' {result = [], errors = []}

showHelp :: IO ()
showHelp = putStrLn "implement some help..."

