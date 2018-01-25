{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Interpreter where

import           Clash.Prelude       hiding (concat, length, map, splitAt, (++))
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
import           Prelude             as P
import           Text.Read
import           Text.Show


data ValueP = Sym String
            | NumP Int
            | Chr Char
            | Str String
            | Quot [ValueP]
            deriving (Eq, Ord, Show)
--
data ValueP' = Sym' (Vec 16 Char)
             | NumP' Int
             | Chr' Char
             | Str' (Vec 32 Char)
             | Quot' Q
             | EmptyQ
             deriving (Eq, Ord, Show)

data Q = Q2     (Vec 2     ValueP')
       | Q4     (Vec 4     ValueP')
       | Q8     (Vec 8     ValueP')
       | Q16    (Vec 16    ValueP')
       | Q32    (Vec 32    ValueP')
       | Q64    (Vec 64    ValueP')
       | Q128   (Vec 128   ValueP')
       | Q256   (Vec 256   ValueP')
       | Q512   (Vec 512   ValueP')
       | Q1024  (Vec 1024  ValueP')
       deriving (Eq, Ord, Show)

data V = V2     (Vec 2     Char)
       | V4     (Vec 4     Char)
       | V8     (Vec 8     Char)
       | V16    (Vec 16    Char)
       | V32    (Vec 32    Char)
       | V64    (Vec 64    Char)
       | V128   (Vec 128   Char)
       | V256   (Vec 256   Char)
       | V512   (Vec 512   Char)
       | V1024  (Vec 1024  Char)
       | V2048  (Vec 2048  Char)
       | V4096  (Vec 4096  Char)
       | V8192  (Vec 8192  Char)
       | V16383 (Vec 16383 Char)
       | V32768 (Vec 32768 Char)
       | V65536 (Vec 65536 Char)
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



































