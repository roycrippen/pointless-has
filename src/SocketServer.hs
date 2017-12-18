{-# LANGUAGE OverloadedStrings #-}

module SocketServer
( application
, jsonVocabShow
, jsonResultsShow
) where

import           Control.Monad      (forever)
import           CoreLibrary        (coreDefinitions)
import           Data.Aeson.Text    (encodeToLazyText)
import           Data.Map           as M (fromList, toList)
import           Data.Maybe         (fromJust)
import           Data.Monoid        (mappend)
import           Data.Text          (Text)
import qualified Data.Text          as T (isPrefixOf, pack, replace, stripPrefix, unpack)
import qualified Data.Text.IO       as T (putStrLn)
import qualified Data.Text.Lazy     as TL (toStrict)
import           Interpreter        (Lang (..), Mode (..), Vocabulary, formatStack, formatWordP,
                                     runQuotation)
import qualified Network.WebSockets as WS (Connection, ServerApp, acceptRequest, forkPingThread,
                                           receiveData, sendTextData)
import           Parser             (parse)
import           PointlessParser    (nakedQuotations)

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

                -- create vocabulary
                let vcab = M.fromList coreDefinitions

                -- listen for commands forever
                talk (Lang vcab [] [] "" WEBSOCKET) conn
            | otherwise -> do
                let err = "incorrect connection topic: '" `mappend` msg `mappend` "'" :: Text
                WS.sendTextData conn err

talk :: Lang -> WS.Connection -> IO ()
talk lang conn = forever $ do
    msg <- WS.receiveData conn
    case msg of
        _
            | T.isPrefixOf "load:" msg -> do
                -- acknowledge load
                let ack = "{\"load\": \"source loaded by pointless engine\"}"
                WS.sendTextData conn (T.pack ack :: Text)

                -- process request
                let source  = fromJust $ T.stripPrefix "load:" msg
                    source' = T.unpack $ T.replace (T.pack "\\n") (T.pack  "\n") source
                    (qs, _):_ = parse nakedQuotations source'
                    vcab' = M.fromList $  coreDefinitions
                    lang' = runQuotation qs (lang { vocab = vcab' })
                    resultsJSON' = jsonResultsShow lang'

                -- T.putStrLn results
                WS.sendTextData conn (resultsJSON' :: Text)

                -- always send updated vocabulary
                -- todo send only on def once tx is implemented
                WS.sendTextData conn  (T.pack $ jsonVocabShow (vocab lang') :: Text)

                -- re-start talk with new vocabulary
                talk (Lang (vocab lang') [] [] "" WEBSOCKET) conn

            | T.isPrefixOf "run:" msg -> do
                T.putStrLn msg
                let source  = fromJust $ T.stripPrefix "run:" msg
                    source' = T.unpack $ T.replace (T.pack "\\n") (T.pack  "\n") source
                    (qs, _):_ = parse nakedQuotations source'
                    -- T.putStrLn $ T.unlines $ Prelude.map (T.pack . formatV) qs
                    lang' = runQuotation qs lang
                    resultsJSON = jsonResultsShow lang'

                -- T.putStrLn results
                WS.sendTextData conn (resultsJSON :: Text)

                -- always send updated vocabulary
                -- todo send only on def once tx is implemented
                WS.sendTextData conn  (T.pack $ jsonVocabShow (vocab lang') :: Text)

                -- re-start talk with new vocabulary
                talk (Lang (vocab lang') [] [] "" WEBSOCKET) conn

            | otherwise -> WS.sendTextData conn ("unknown topic" :: Text)


-- | Serializes a Lang to JSON.
jsonResultsShow :: Lang -> Text
jsonResultsShow lang = T.pack "{\n" `mappend` text `mappend` T.pack "\n}"
  where
    stackT   = encodeP "\"stack\":" (formatStack $ stack lang)
    resultT  = encodeP "\"result\":" (result lang)
    displayT = encodeP "\"display\":" [display lang]
    newline  = T.pack ",\n"
    text     = stackT `mappend` newline
                      `mappend` resultT
                      `mappend` newline
                      `mappend` displayT

encodeP :: String -> [String] -> Text
encodeP s xs = T.pack s `mappend` TL.toStrict (encodeToLazyText xs)

jsonVocabElementShow :: Vocabulary -> String
jsonVocabElementShow vcab = jsonArrayElementShow "vocab" vocab'
  where
    vocab' = map (\(k, v) -> k ++ " == " ++ formatWordP v) $ M.toList vcab

jsonVocabShow :: Vocabulary -> String
jsonVocabShow = jsonWrapElement . jsonVocabElementShow

jsonArrayElementShow :: String -> [String] -> String
jsonArrayElementShow name xs = "\"" ++ name ++ "\":[ " ++ bodyTrimmed ++ " ]"
  where
    body        = foldl (\acc v -> acc ++ show v ++ ", ") "" xs
    bodyTrimmed = take (length body - 2) body

jsonWrapElement :: String -> String
jsonWrapElement s = "{\n" ++ s ++ "\n}"


