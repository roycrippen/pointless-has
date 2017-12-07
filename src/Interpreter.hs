module Interpreter where

import           Data.Aeson
import qualified Data.ByteString.Lazy       as B ()
import qualified Data.ByteString.Lazy.Char8 as BC (unpack)
import qualified Data.Map                   as M (Map, lookup, toList)
import           Numeric                    (showFFloat)

data ValueP = Symbol String
           | NumP Double
           | Chr Char
           | Str String
           | Quot [ValueP]
    deriving (Eq, Ord, Show)

data Lang = Lang { vocab  :: Vocabulary
                 , stack  :: [ValueP]
                 , result :: [String]
                 , errors :: [String]
                 }
                 deriving (Show)

data WordP = Quotation [ValueP]
           | Function (Lang -> Lang)

instance Show WordP where
    show = formatWordP

type Vocabulary = M.Map String WordP

getWord :: String -> Vocabulary -> Maybe WordP
getWord = M.lookup

isTrue :: ValueP -> Bool
isTrue (NumP x)   = x /= 0.0
isTrue (Quot   q) = not (null q)
isTrue _          = False

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

-- pretty-prints
formatStack :: [ValueP] -> String
formatStack = unlines . map ((\ s -> if s == "" then "\"\"" else s) . formatV)

formatV :: ValueP -> String
formatV (Symbol s) = s
formatV (NumP n) = if isInteger then show (truncate n :: Integer) else floatStr
 where
  properFraction' :: Double -> (Integer, Double)
  properFraction' = properFraction
  (_, realFrac)   = properFraction' n
  isInteger       = abs realFrac < 0.00000001
  floatStr        = showFFloat (Just 6) n ""
formatV (Quot []) = "[]"
formatV (Quot q ) = concat ["[ ", unwords $ map formatV q, " ]"]
formatV (Chr c)   = [c]
formatV (Str s)   = s

formatWordP :: WordP -> String
formatWordP (Quotation xs) = formatV (Quot xs)
formatWordP (Function  _ ) = "Primitive function"

formatWordAST :: WordP -> String
formatWordAST (Quotation xs) = show xs
formatWordAST (Function  _ ) = "function: Vocabulary -> [ValueP] -> [ValueP]"

jsonResultsShow :: Lang -> String
jsonResultsShow lang = "{\n" ++ ssStr ++ dsStr ++ esStr ++ "\n}"
 where
  ssStr = jsonStackElementShow (stack lang) ++ ",\n"
  dsStr = "\"result\":" ++ encodeP (result lang) ++ ",\n"
  esStr = "\"errors\":" ++ encodeP (errors lang)
--   dsStr = jsonArrayElementShow "result" (result lang) ++ ",\n"
--   esStr = jsonArrayElementShow "errors" (errors lang)

encodeP :: [String] -> String
encodeP xs = BC.unpack $ encode xs

jsonStackElementShow :: [ValueP] -> String
jsonStackElementShow stck = jsonArrayElementShow "stack" (split ',' stack')
    where stack' = map (\c -> if c == '\n' then ',' else c) $ formatStack stck

jsonStackShow :: [ValueP] -> String
jsonStackShow = jsonWrapElement . jsonStackElementShow

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

split :: Char -> String -> [String]
split _ "" = []
split c s  = l : case s' of
    []      -> []
    (_:s'') -> split c s''
    where (l, s') = break (==c) s


