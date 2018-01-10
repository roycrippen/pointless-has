{-# LANGUAGE OverloadedStrings #-}

module SocketServer
( application
, jsonVocabShow
) where

import           Control.Monad      (forever)
import           Core               (coreDefinitions)
-- import           Data.Aeson.Text    (encodeToLazyText)
import           Data.Map           as M (toList)
import           Data.Maybe         (fromJust)
import           Data.Monoid        (mappend)
import           Data.Text          (Text)
import qualified Data.Text          as T (isPrefixOf, pack, replace, stripPrefix, unpack)
import qualified Data.Text.IO       as T (putStrLn)
-- import qualified Data.Text.Lazy     as TL (toStrict)
import           Interpreter        (Lang (..), Mode (..), Vocabulary, formatWordP, runQuotation)
import qualified Network.WebSockets as WS (Connection, ServerApp, acceptRequest, forkPingThread,
                                           receiveData, sendTextData)
-- import           Parser             (parse)
-- import           PointlessParser    (nakedQuotations)
import           Primitives         (jsonResultsShow, runQuotStr)

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

        -- listen for commands forever
        talk (Lang coreDefinitions [] [] "" (WEBSOCKET conn)) conn
      | otherwise -> do
        let
          err =
            "incorrect connection topic: '" `mappend` msg `mappend` "'" :: Text
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
        let source       = fromJust $ T.stripPrefix "load:" msg
            source' = T.unpack $ T.replace (T.pack "\\n") (T.pack "\n") source
            lang'        = runQuotStr source' (lang { vocab = coreDefinitions })
            resultsJSON' = jsonResultsShow lang'

        WS.sendTextData conn (resultsJSON' :: Text)

        -- always send updated vocabulary
        -- todo send only on def once tx is implemented
        WS.sendTextData conn (T.pack $ jsonVocabShow (vocab lang') :: Text)

        -- re-start talk with new vocabulary
        talk            (Lang (vocab lang') [] [] "" (WEBSOCKET conn)) conn
      | T.isPrefixOf "run:" msg -> do
        T.putStrLn msg
        let source      = fromJust $ T.stripPrefix "run:" msg
            source' = T.unpack $ T.replace (T.pack "\\n") (T.pack "\n") source
            lang'       = runQuotStr source' lang
            resultsJSON = jsonResultsShow lang'

        WS.sendTextData conn (resultsJSON :: Text)

        -- always send updated vocabulary
        -- todo send only on def once tx is implemented
        WS.sendTextData conn (T.pack $ jsonVocabShow (vocab lang') :: Text)

        -- re-start talk with new vocabulary
        talk            (Lang (vocab lang') [] [] "" (WEBSOCKET conn)) conn
      | otherwise -> WS.sendTextData conn ("unknown topic" :: Text)


jsonVocabElementShow :: Vocabulary -> String
jsonVocabElementShow vcab = jsonArrayElementShow "vocab" vocab'
  where vocab' = map (\(k, v) -> k ++ " == " ++ formatWordP v) $ M.toList vcab

jsonVocabShow :: Vocabulary -> String
jsonVocabShow = jsonWrapElement . jsonVocabElementShow

jsonArrayElementShow :: String -> [String] -> String
jsonArrayElementShow name xs = "\"" ++ name ++ "\":[ " ++ bodyTrimmed ++ " ]"
 where
  body        = foldl (\acc v -> acc ++ show v ++ ", ") "" xs
  bodyTrimmed = take (length body - 2) body

jsonWrapElement :: String -> String
jsonWrapElement s = "{\n" ++ s ++ "\n}"








