module Interpreter where

import qualified Data.Map   as M (Map, lookup)
import           Data.Maybe (fromMaybe)


data Value = Symbol String | Number Double | Quot Stack
    deriving (Eq,Show)

type Stack = [Value]

data WordP = Quotation Stack                           -- composite word
           | Primitive (Stack -> Stack)                -- pure stack effect function
           | Function (Vocabulary -> Stack -> Stack)   -- function requiring vocabulary

-- data WordP = Quotation Stack                                  -- composite word
--             | Primitive (Stack -> Stack)                      -- simple, pure stack effect
--             | EnvPrimitive (Vocabulary -> Stack -> IO Stack)  -- function needing the vocabulary

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

-- -- run the given word, whether given as a quotation or a primitive
-- runWord :: WordP -> Vocabulary -> Stack -> IO Stack
-- runWord w env stack = case w of
--     Quotation q    -> runQuotation q env stack
--     Primitive f    -> return $ f stack
--     EnvPrimitive f -> f env stack

-- runInstruction :: Value -> Vocabulary -> Stack -> IO Stack
-- runInstruction ins env stack = case ins of
--     Symbol w -> runWord (getWord w env) env stack
--     x        -> return (x:stack)

-- runQuotation :: Stack -> Vocabulary -> Stack -> IO Stack
-- runQuotation [] _ s = return s
-- runQuotation (i:is) env s = do
--     s' <- runInstruction i env s
--     runQuotation is env s'

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
