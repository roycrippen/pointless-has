{-# LANGUAGE OverloadedStrings #-}

module SocketServer
    ( application
    , talk
    ) where

import           Control.Monad      (forever)
import           CoreLibrary        (coreDefinitions, getQuotations)
import           Data.Map           as M
import           Data.Maybe         (fromJust)
import           Data.Monoid        (mappend)
import           Data.Text          (Text)
import qualified Data.Text          as T (isPrefixOf, pack, stripPrefix, unpack)
import qualified Data.Text.IO       as T (putStrLn)
import           Interpreter        (Lang (..), Vocabulary, formatStack, jsonResultsShow, jsonVocabShow, runQuotation)
import qualified Network.WebSockets as WS (Connection, ServerApp, acceptRequest, forkPingThread, receiveData,
                                           sendTextData)
import           Parser             (parse)
import           PointlessParser    (nakedQuotations)
import           Primitives         (primitives)

application :: WS.ServerApp
application pending = do
    T.putStrLn "pointless websocket server started"
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    case msg of
        _
            | T.isPrefixOf "pointless_connection" msg -> do
                WS.sendTextData conn ("pointless" :: Text)
                T.putStrLn "editor connected"
                let coreLibrary = getQuotations coreDefinitions
                    vcab        = M.fromList $ primitives ++ coreLibrary
                WS.sendTextData conn (T.pack $ jsonVocabShow vcab :: Text)
                talk            vcab conn
            | otherwise -> do
                let
                    err =
                        "incorrect connection topic: '"
                        `mappend` msg
                        `mappend` "'" :: Text
                WS.sendTextData conn err

talk :: Vocabulary -> WS.Connection -> IO ()
talk vcab conn = forever $ do
    msg <- WS.receiveData conn
    case msg of
        _
            | T.isPrefixOf "run:" msg
                -- T.putStrLn (T.pack (show quots))
                -- T.putStrLn $ T.pack $ jsonResultsShow lang
                                      -> do
                T.putStrLn $ T.pack "run message recieved: " `mappend` msg
                let qStr       = T.unpack $ fromJust $ T.stripPrefix "run:" msg
                    (quots, _) = head $ parse nakedQuotations qStr
                    lang       = runQuotation quots (Lang vcab [] [] [])
                    results    = T.pack $ jsonResultsShow lang
                WS.sendTextData conn (results :: Text)
            | otherwise -> WS.sendTextData conn ("unknown topic" :: Text)


-- getProgram :: IO ([(String, WordP)], Stack)
-- getProgram = do
--     source <- readFile "data/test.joy"
--     let ((defs, quots), _) = head $ parse program source
--         coreLibrary        = getQuotations coreDefinitions
--     return (primitives ++ coreLibrary ++ defs, quots)


































































































