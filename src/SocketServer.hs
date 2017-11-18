{-# LANGUAGE OverloadedStrings #-}

module SocketServer where

import           Control.Monad      (forever)
import           CoreLibrary
import           Data.Map           as M
import           Data.Maybe
import           Data.Monoid        (mappend)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Interpreter
import qualified Network.WebSockets as WS
import           Parser
import           PointlessParser
import           Primitives

main :: IO ()
main = do
    WS.runServer "127.0.0.1" 9160 application
    T.putStrLn "pointless websocket server started"

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    case msg of
        _
            | T.isPrefixOf "pointless_connection" msg -> do
                WS.sendTextData conn ("pointless" :: Text)
                T.putStrLn "editor connected"

                -- setup and send vocabulary of functions
                let coreLibrary = getQuotations coreDefinitions
                    vcab        = M.fromList $ primitives ++ coreLibrary
                WS.sendTextData conn (T.pack $ jsonVocabShow vcab :: Text)

                talk            vcab conn
            | otherwise -> WS.sendTextData
                conn
                ( "incorrect connection topic: '" `mappend` msg `mappend` "'" :: Text
                )

talk :: Vocabulary -> WS.Connection -> IO ()
talk vcab conn = forever $ do
    msg <- WS.receiveData conn
    case msg of
        _
            | T.isPrefixOf "run:" msg -> do
                let qStr       = T.unpack $ fromJust $ T.stripPrefix "run:" msg
                    (quots, _) = head $ parse nakedQuotations qStr
                    lang       = runQuotation quots (Lang vcab [] [] [])
                    results    = T.pack $ jsonResultsShow lang
                WS.sendTextData conn (results :: Text)
                T.putStrLn $ T.pack "run message recieved: " `mappend` msg
            | otherwise -> WS.sendTextData conn ("unknown topic" :: Text)











































