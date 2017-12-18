module Main where

import qualified Network.WebSockets as WS (runServer)
import           Repl               (startRepl)
import           SocketServer       (application)
import           System.Environment (getArgs)
import           System.Exit        (exitSuccess)

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
        print args
        putStrLn "invalid arg(s), use \"web\" for wesocket server or no arg for repl"
        exitSuccess
    _ -> do
        startRepl
