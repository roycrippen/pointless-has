module Interpreter where

import           Data.Char (chr)
import qualified Data.Map  as M (Map, lookup)
import           Numeric   (showFFloat)

data ValueP = Symbol String
            | NumP Double
            | Chr Char
            | Str String
            | Quot [ValueP]
            deriving (Eq, Ord, Show)

data Lang = Lang { vocab   :: Vocabulary
                 , stack   :: [ValueP]
                 , result  :: [String]
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
        Nothing -> lang { result = msg : result lang }
            where msg = "ERROR(getWord): not a valid word " ++ show w
    x -> lang { stack = x : stack lang }

-- |
-- | pretty printers
-- |

formatV :: ValueP -> String
formatV (Symbol s) = s
formatV (NumP n) = if isInteger n then show (truncate n :: Integer) else floatStr
  where floatStr        = showFFloat (Just 6) n ""
formatV (Quot []) = "[]"
formatV (Quot q ) = concat ["[ ", unwords $ map formatV q, " ]"]
formatV (Chr  c ) = [c]
formatV (Str  s ) =  show s

formatPutch :: ValueP -> Maybe Char
formatPutch (NumP n) = if isInteger n then Just charFromInt else Nothing
  where charFromInt = chr (truncate n :: Int)
formatPutch (Chr  c ) = Just c
formatPutch _ = Nothing

formatWordP :: WordP -> String
formatWordP (Quotation xs) = formatV (Quot xs)
formatWordP (Function  _ ) = "Primitive function"

formatWordAST :: WordP -> String
formatWordAST (Quotation xs) = show xs
formatWordAST (Function  _ ) = "function: Vocabulary -> [ValueP] -> [ValueP]"

formatStack :: [ValueP] -> [String]
formatStack = map formatV

isInteger :: Double -> Bool
isInteger d = abs realFrac < 0.0000001
  where (_, realFrac) = properFraction' d
        properFraction' :: Double -> (Integer, Double)
        properFraction' = properFraction

