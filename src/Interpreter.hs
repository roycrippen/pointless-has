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

data WordP = Quotation Stack                           -- composite word
           | Primitive (Stack -> Stack)                -- pure stack effect function
           | Function (Vocabulary -> Stack -> Stack)   -- function requiring vocabulary

instance Show WordP where
    show = formatWordP

type Vocabulary = M.Map String WordP

getWord :: String -> Vocabulary -> WordP
getWord w vocab = fromMaybe (error $ "undefined word " ++ w) (M.lookup w vocab)

isTrue :: Value -> Bool
isTrue (Number x) = x /= 0.0
isTrue (Quot q)   = not (null q)

toTruth :: Bool -> Value
toTruth b = if b
                then Number 1.0
                else Number 0.0

runWord :: WordP -> Vocabulary -> Stack -> Stack
runWord w vocab stack = case w of
    Quotation q -> runQuotation q vocab stack
    Primitive f -> f stack
    Function f  -> f vocab stack

runQuotation :: Stack -> Vocabulary -> Stack -> Stack
runQuotation [] _ s = s
runQuotation (i:is) vocab s = runQuotation is vocab s'
  where
    s' = runInstruction i vocab s

runInstruction :: Value -> Vocabulary -> Stack -> Stack
runInstruction ins vocab stack = case ins of
    Symbol w -> runWord (getWord w vocab) vocab stack
    x        -> x:stack

quotCons :: Value -> Value -> Value
quotCons x (Quot q) = Quot (x:q)
quotCons _ _        = error "Error in cons, second argument not a quotation"

-- pretty-prints
formatStack :: Stack -> String
formatStack = unlines . map formatV

formatV :: Value -> String
formatV (Symbol s) = s
formatV (Number n) = if isInteger
    then show (truncate n :: Integer)
    else show n
  where
    properFraction' :: Double -> (Integer, Double)
    properFraction' = properFraction
    (_, realFrac) = properFraction' n
    isInteger = realFrac < 0.00000001
formatV (Quot [])  = "[]"
formatV (Quot q)   = concat ["[ ", unwords $ map formatV q, " ]"]

formatWordP :: WordP -> String
formatWordP (Quotation xs) = formatV (Quot xs)
formatWordP (Primitive _)  = "function: Stack -> Stack"
formatWordP (Function _)   = "function: Vocabulary -> Stack -> Stack"

formatWordAST :: WordP -> String
formatWordAST (Quotation xs) = show xs
formatWordAST (Primitive _)  = "function: Stack -> Stack"
formatWordAST (Function _)   = "function: Vocabulary -> Stack -> Stack"

jsonLangShow :: Lang -> String
jsonLangShow lang =
    "{\n" ++ vsStr ++ ",\n" ++ ssStr ++ ",\n" ++ dsStr ++ ",\n" ++ esStr ++ "\n}"
  where
    vocab' = map (\(k, v) -> k ++ " == " ++ (formatWordP v)) $ M.toList $ vocab lang
    vsStr = jsonArrayShow "display" vocab'
    stack' =  map (\c -> if c == '\n' then ',' else c) $ formatStack (stack lang)
    ssStr =  "\"stack\": [" ++  stack' ++ "]"
    dsStr = jsonArrayShow "display" $ display lang
    esStr = jsonArrayShow "errors" $ errors lang

jsonArrayShow :: String -> [String] -> String
jsonArrayShow name xs = "\"" ++ name ++ "\":[ " ++ bodyTrimmed ++ " ]"
  where
    body = (foldl (\acc v -> acc ++ (show v) ++ ", ") "" xs)
    bodyTrimmed = take (length body - 2)  body
