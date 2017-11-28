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
import           Interpreter        (Lang (..), Stack, Vocabulary, formatStack,
                                     jsonResultsShow, jsonVocabShow,
                                     runQuotation)
import qualified Network.WebSockets as WS (Connection, ServerApp, acceptRequest,
                                           forkPingThread, receiveData,
                                           sendTextData)
import           Parser             (parse)
import           PointlessParser    (nakedQuotations, program)
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

                -- give editor the vocabulary
                let coreLibrary = getQuotations coreDefinitions
                    vcab        = M.fromList $ primitives ++ coreLibrary
                WS.sendTextData conn (T.pack $ jsonVocabShow vcab :: Text)

                -- listen for commands forever
                talk vcab conn
            | otherwise -> do
                let err = "incorrect connection topic: '" `mappend` msg `mappend` "'" :: Text
                WS.sendTextData conn err

talk :: Vocabulary -> WS.Connection -> IO ()
talk vcab conn = forever $ do
    msg <- WS.receiveData conn
    case msg of
        _
            | T.isPrefixOf "load:" msg -> do
                -- acknowledge load
                let ack = "{\"load\": \"source loaded by pointless engine\"}"
                WS.sendTextData conn (T.pack ack :: Text)

                -- process request
                let source = T.unpack $ fromJust $ T.stripPrefix "load:" msg
                    ((ds, qs), _) = head $ parse program source
                    vcab' =  M.fromList $ getQuotations coreDefinitions ++ primitives ++ ds
                process qs vcab' conn

                -- send updated vocabulary
                WS.sendTextData conn (T.pack $ jsonVocabShow vcab' :: Text)

                -- re-start talk with new vocabulary
                talk vcab' conn
            | T.isPrefixOf "run:" msg -> do
                -- run single quote
                T.putStrLn msg
                let source  = T.unpack $ fromJust $ T.stripPrefix "run:" msg
                    (qs, _) = head $ parse nakedQuotations source
                process qs vcab conn
            | otherwise -> WS.sendTextData conn ("unknown topic" :: Text)

process :: Stack -> Vocabulary -> WS.Connection -> IO ()
process qs vcab conn = do
    let lang    = runQuotation qs (Lang vcab [] [] [])
        results = T.pack $ jsonResultsShow lang
    WS.sendTextData conn (results :: Text)



-- getProgram :: IO ([(String, WordP)], Stack)
-- getProgram = do
--     source <- readFile "data/test.joy"
--     let ((defs, quots), _) = head $ parse program source
--         coreLibrary        = getQuotations coreDefinitions
--     return (primitives ++ coreLibrary ++ defs, quots)

-- T.putStrLn (T.pack (show quots))
-- T.putStrLn $ T.pack $ jsonResultsShow lang































































































































