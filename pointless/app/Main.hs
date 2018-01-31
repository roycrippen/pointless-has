module Main where

-- import qualified Network.WebSockets as WS (runServer)
-- import           Repl               (startRepl)
-- import           SocketServer       (application)
-- import           System.Environment (getArgs)
-- import           System.Exit        (exitSuccess)
-- import CLaSH.Prelude
import Interpreter
import Parser
import NonHW
-- import Prelude as P (length, (++))

main :: IO ()
main = do
  putStrLn "Pointless...\n\n"
  let res = case parse nakedQuotations (V16384 longV) of
              Nothing     -> "Nothing"
              Just (v, _) -> show v
  print $ length res

  -- let res = showParse $ parse nakedQuotations (loadStr (replaceStr "\\n" "\n" longSrc))
  -- putStrLn $ "length of result = " P.++ show (P.length res)

  -- args <- getArgs
  -- case args of
  --   x:_ -> case x of
  --     "web" -> do
  --       putStrLn "Welcome to the Pointless web server\n"
  --       putStrLn "starting websocket server"
  --       WS.runServer "127.0.0.1" 9160 application
  --     _ -> do
  --       print args
  --       putStrLn "invalid arg(s), use \"web\" for wesocket server or no arg for repl"
  --       exitSuccess
  --   _ -> startRepl




