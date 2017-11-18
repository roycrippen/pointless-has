module Interpreter where

import qualified Data.Map   as M (Map, lookup, toList)
import           Data.Maybe (fromMaybe)


data Value = Symbol String | Number Double | Quot Stack
    deriving (Eq,Show)

type Stack = [Value]

data Lang = Lang { vocab   :: Vocabulary
                 , stack   :: Stack
                 , display :: [String]
                 , errors  :: [String]
                 }
                 deriving (Show)

data WordP = Quotation Stack          -- composite word
           | Function (Lang -> Lang)  -- function to execute

instance Show WordP where
    show = formatWordP

type Vocabulary = M.Map String WordP

getWord :: String -> Vocabulary -> WordP
getWord w vcab = fromMaybe (error $ "undefined word " ++ w) (M.lookup w vcab)

isTrue :: Value -> Bool
isTrue (Number x) = x /= 0.0
isTrue (Quot   q) = not (null q)
isTrue _          = False

toTruth :: Bool -> Value
toTruth b = if b then Number 1.0 else Number 0.0

runWord :: WordP -> Lang -> Lang
runWord w lang = case w of
    Quotation q -> runQuotation q lang
    Function  f -> f lang

runQuotation :: Stack -> Lang -> Lang
runQuotation quotation lang = case quotation of
    []     -> lang
    (i:is) -> runQuotation is lang' where lang' = runInstruction i lang

runInstruction :: Value -> Lang -> Lang
runInstruction ins lang = case ins of
    Symbol w -> runWord (getWord w (vocab lang)) lang
    x        -> lang { stack = x : stack lang }

quotCons :: Value -> Value -> Value
quotCons x (Quot q) = Quot (x : q)
quotCons _ _        = error "Error in cons, second argument not a quotation"

-- pretty-prints
formatStack :: Stack -> String
formatStack = unlines . map formatV

formatV :: Value -> String
formatV (Symbol s) = s
formatV (Number n) = if isInteger then show (truncate n :: Integer) else show n
 where
  properFraction' :: Double -> (Integer, Double)
  properFraction' = properFraction
  (_, realFrac)   = properFraction' n
  isInteger       = realFrac < 0.00000001
formatV (Quot []) = "[]"
formatV (Quot q ) = concat ["[ ", unwords $ map formatV q, " ]"]

formatWordP :: WordP -> String
formatWordP (Quotation xs) = formatV (Quot xs)
formatWordP (Function  _ ) = "Primitive function"

formatWordAST :: WordP -> String
formatWordAST (Quotation xs) = show xs
formatWordAST (Function  _ ) = "function: Vocabulary -> Stack -> Stack"

jsonResultsShow :: Lang -> String
jsonResultsShow lang = "{\n" ++ ssStr ++ dsStr ++ esStr ++ "\n}"
 where
  ssStr = jsonStackElementShow (stack lang) ++ ",\n"
  dsStr = jsonArrayElementShow "display" (display lang) ++ ",\n"
  esStr = jsonArrayElementShow "errors" (errors lang)

jsonStackElementShow :: Stack -> String
jsonStackElementShow stck = jsonArrayElementShow "stack" (split ',' stack')
    where stack' = map (\c -> if c == '\n' then ',' else c) $ formatStack stck

jsonStackShow :: Stack -> String
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


































































































