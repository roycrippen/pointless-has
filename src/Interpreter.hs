module Interpreter where

import           Data.Aeson.Text (encodeToLazyText)
import           Data.Char       (chr)
import qualified Data.Map        as M (Map, lookup, toList)
import           Data.Text       (Text)
import qualified Data.Text       as T (pack, replace)
import qualified Data.Text.Lazy  as TL (toStrict)
import           Numeric         (showFFloat)


data ValueP = Symbol String
            | NumP Double
            | Chr Char
            | Str String
            | Quot [ValueP]
            deriving (Eq, Ord, Show)

data Lang = Lang { vocab   :: Vocabulary
                 , stack   :: [ValueP]
                 , result  :: [String]
                 , errors  :: [String]
                 , display :: String
                 }
                 deriving (Show)

data WordP = Quotation [ValueP] | Function (Lang -> Lang)
instance Show WordP where show = formatWordP

type Vocabulary = M.Map String WordP

getWord :: String -> Vocabulary -> Maybe WordP
getWord = M.lookup

isTrue :: ValueP -> Bool
isTrue (NumP x) = x /= 0.0
isTrue (Quot q) = not (null q)
isTrue _        = False

toTruth :: Bool -> ValueP
toTruth b = if b then NumP 1.0 else NumP 0.0

runWord :: WordP -> Lang -> Lang
runWord w lang = case w of
    Quotation q -> runQuotation q lang
    Function  f -> f lang

runQuotation :: [ValueP] -> Lang -> Lang
runQuotation quotation lang = case quotation of
    []     -> lang
    (i:is) -> runQuotation is (runInstruction i lang)

runInstruction :: ValueP -> Lang -> Lang
runInstruction ins lang = case ins of
    Symbol w -> case getWord w (vocab lang) of
        Just w' -> runWord w' lang
        Nothing -> lang { errors = msg : errors lang }
            where msg = "getWord: not a valid word " ++ show w
    x -> lang { stack = x : stack lang }

quotCons :: ValueP -> ValueP -> ValueP
quotCons x (Quot q) = Quot (x : q)
quotCons _ _        = error "Error in cons, second argument not a quotation"

--
-- pretty printers
--

properFraction' :: Double -> (Integer, Double)
properFraction' = properFraction

formatV :: ValueP -> String
formatV (Symbol s) = s
formatV (NumP n) = if isInteger then show (truncate n :: Integer) else floatStr
  where
    (_, realFrac)   = properFraction' n
    isInteger       = abs realFrac < 0.00000001
    floatStr        = showFFloat (Just 6) n ""
formatV (Quot []) = "[]"
formatV (Quot q ) = concat ["[ ", unwords $ map formatV q, " ]"]
formatV (Chr  c ) = [c]
formatV (Str  s ) = show s

formatPutch :: ValueP -> Maybe Char
formatPutch (NumP n) = if isInteger then Just charFromInt else Nothing
  where
    (_, realFrac)   = properFraction' n
    isInteger       = abs realFrac < 0.00000001
    charFromInt = chr (truncate n :: Int)
formatPutch (Chr  c ) = Just c
formatPutch _ = Nothing

formatWordP :: WordP -> String
formatWordP (Quotation xs) = formatV (Quot xs)
formatWordP (Function  _ ) = "Primitive function"

formatWordAST :: WordP -> String
formatWordAST (Quotation xs) = show xs
formatWordAST (Function  _ ) = "function: Vocabulary -> [ValueP] -> [ValueP]"

formatStack :: [ValueP] -> [String]
formatStack xs  = map (\x -> formatV x) xs

-- |
-- | json formatters
-- |

-- | Serializes a Lang to JSON.
jsonResultsShow :: Lang -> Text
jsonResultsShow lang = T.pack "{\n" `mappend` text `mappend` T.pack "\n}"
  where
    stackT   = encodeP "\"stack\":" (formatStack $ stack lang)
    resultT  = encodeP "\"result\":" (reverse $ result lang)
    errorT   = encodeP "\"errors\":" (errors lang)
    displayT = encodeP "\"display\":" [display lang]
    newline  = T.pack ",\n"
    text     = stackT `mappend` newline
                      `mappend` resultT
                      `mappend` newline
                      `mappend` errorT
                      `mappend` newline
                      `mappend` displayT

encodeP :: String -> [String] -> Text
encodeP s xs = T.replace (T.pack "\\n") (T.pack "\n") encoded
  where encoded = T.pack s `mappend` TL.toStrict (encodeToLazyText xs)

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

jsonArrayShow :: String -> [String] -> String
jsonArrayShow name xs = jsonWrapElement $ jsonArrayElementShow name xs

jsonWrapElement :: String -> String
jsonWrapElement s = "{\n" ++ s ++ "\n}"




