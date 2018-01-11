module Interpreter where

import           CLaSH.Prelude       hiding (concat, length, many, map, splitAt, (++), (<|>))

import           Control.Applicative (Applicative (..), pure)
import           Control.Monad       (Functor (..), Monad (..), ap, liftM, void)
import           Data.Bool
import           Data.Char
import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.List           as L
import qualified Data.Map            as M (Map, lookup)
import           Data.Maybe          (isJust)
import           Data.String
import           Text.Read
import           Text.Show

data ValueP = Sym String
            | NumP Int
            | Chr Char
            | Str String
            | Quot [ValueP]
            deriving (Eq, Ord, Show)

data Lang = Lang { vocab   :: Vocabulary
                 , stack   :: [ValueP]
                 , result  :: [String]
                 , display :: String
                 , mode    :: Mode
                 }
                 deriving (Show)

data WordP = Quotation [ValueP] | Function (Lang -> Lang)
instance Show WordP where show = formatWordP

data Mode = REPL | WEBSOCKET
instance Show Mode where show = formatMode

type Vocabulary = M.Map String WordP

getWord :: String -> Vocabulary -> Maybe WordP
getWord = M.lookup

isTrue :: ValueP -> Bool
isTrue (NumP x) = x /= 0
isTrue (Quot q) = not (null q)
isTrue _        = False

toTruth :: Bool -> ValueP
toTruth b = if b then NumP 1 else NumP 0

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
  Sym w -> case getWord w (vocab lang) of
    Just w' -> runWord w' lang
    Nothing -> lang { result = msg : result lang }
      where msg = "ERROR(getWord): not a valid word " ++ show w
  x -> lang { stack = x : stack lang }

-- |
-- | pretty printers
-- |

formatV :: ValueP -> String
formatV (Sym  s ) = s
-- formatV (NumP   n) = if isInteger n
--   then show (truncate n :: Integer)
--   else floatStr
--   where floatStr = showFFloat (Just 6) n ""
formatV (NumP n ) = show n
formatV (Quot []) = "[]"
formatV (Quot q ) = concat ["[ ", unwords $ map formatV q, " ]"]
formatV (Chr  c ) = [c]
formatV (Str  s ) = show s

formatPutch :: ValueP -> Maybe Char
-- formatPutch (NumP n) = if isInteger n then Just charFromInt else Nothing
-- where charFromInt = chr (truncate n :: Int)
formatPutch (NumP n) = Just $ chr n
formatPutch (Chr  c) = Just c
formatPutch _        = Nothing

formatWordP :: WordP -> String
formatWordP (Quotation xs) = formatV (Quot xs)
formatWordP (Function  _ ) = "Primitive function"

formatWordAST :: WordP -> String
formatWordAST (Quotation xs) = show xs
formatWordAST (Function  _ ) = "function: Vocabulary -> [ValueP] -> [ValueP]"

formatStack :: [ValueP] -> [String]
formatStack = map formatV

-- isInteger :: Double -> Bool
-- isInteger d = abs realFrac < 0.0000001
--  where
--   (_, realFrac) = properFraction' d
--   properFraction' :: Double -> (Integer, Double)
--   properFraction' = properFraction

formatMode :: Mode -> String
formatMode REPL      = "REPL mode"
formatMode WEBSOCKET = "Websocket mode"

replaceStr :: String -> String -> String -> String
replaceStr _   _   []  = []
replaceStr old new str = go str
 where
  go [] = []
  go str'@(x:xs) =
    let (prefix, rest) = splitAt n str'
    in  if old == prefix then new ++ go rest else x : go xs
  n = length old














