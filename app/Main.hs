module Main where

import qualified Network.WebSockets as WS (runServer)
import           SocketServer       (application)

-- import           Parser
-- import           PointlessParser


main :: IO ()
main = do
    putStrLn "starting websocket server"
    WS.runServer "127.0.0.1" 9160 application

-- main = do
--     let (q, s) = head $ parse nakedQuotations "[]"
--     print q
--     putStrLn "done"





























