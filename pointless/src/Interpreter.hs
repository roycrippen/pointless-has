{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Interpreter where

import           Clash.Prelude
import           Control.Applicative (Applicative (..), pure)
import           Control.Monad       (Functor (..), Monad (..), ap, liftM, void)
import           Data.Bool
import           Data.Char
import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.List           as L (concat, map, length, splitAt)
import qualified Data.Map            as M (Map, lookup)
import           Data.Maybe          (isJust)
import           Data.String
import           Prelude             as P ((++))
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
      where msg = "ERROR(getWord): not a valid word " P.++ show w
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
formatV (Quot q ) = L.concat ["[ ", unwords $ L.map formatV q, " ]"]
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
formatStack = L.map formatV

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
    let (prefix, rest) = L.splitAt n str'
    in  if old == prefix then new P.++ go rest else x : go xs
  n = L.length old


-- | size helpers

newLengthVP :: KnownNat n => Vec n ValueP' -> Int
newLengthVP vs =
  2 ^ ceiling (logBase 2 $ fromIntegral (cntConsecutive EmptyQ vs)) :: Int

newLengthC :: KnownNat n => Vec n Char -> Int
newLengthC vs =
  2 ^ ceiling (logBase 2 $ fromIntegral (cntConsecutive '~' vs)) :: Int

-- | Count non '~' consecutive charaters starting a Vector
cntConsecutive :: (Eq a, KnownNat n) => a -> Vec n a -> Int
cntConsecutive a vs = case Clash.Prelude.findIndex (==a) vs of
  Just n -> fromIntegral (toInteger n)
  _      -> Clash.Prelude.length vs

pruneQ :: Q -> Q
pruneQ qs = case qs of
  Q16 vs -> case newLengthVP vs of
    2 -> Q2 (take d2 vs)
    4 -> Q4 (take d4 vs)
    8 -> Q8 (take d8 vs)
    _ -> qs
  Q32 vs -> case newLengthVP vs of
    2  -> Q2 (take d2 vs)
    4  -> Q4 (take d4 vs)
    8  -> Q8 (take d8 vs)
    16 -> Q16 (take d16 vs)
    _  -> qs
  Q64 vs -> case newLengthVP vs of
    2  -> Q2 (take d2 vs)
    4  -> Q4 (take d4 vs)
    8  -> Q8 (take d8 vs)
    16 -> Q16 (take d16 vs)
    32 -> Q32 (take d32 vs)
    _  -> qs
  Q1024 vs -> case newLengthVP vs of
    2   -> Q2 (take d2 vs)
    4   -> Q4 (take d4 vs)
    8   -> Q8 (take d8 vs)
    16  -> Q16 (take d16 vs)
    32  -> Q32 (take d32 vs)
    64  -> Q64 (take d64 vs)
    128 -> Q128 (take d128 vs)
    256 -> Q256 (take d256 vs)
    512 -> Q512 (take d512 vs)
    _   -> qs
  _ -> qs

--
pruneV :: V -> V
pruneV vvs = case vvs of
  V16 vs -> case newLengthC vs of
    2 -> V2 (take d2 vs)
    4 -> V4 (take d4 vs)
    8 -> V8 (take d8 vs)
    _ -> vvs
  V32 vs -> case newLengthC vs of
    2  -> V2 (take d2 vs)
    4  -> V4 (take d4 vs)
    8  -> V8 (take d8 vs)
    16 -> V16 (take d16 vs)
    _  -> vvs
  V64 vs -> case newLengthC vs of
    2  -> V2 (take d2 vs)
    4  -> V4 (take d4 vs)
    8  -> V8 (take d8 vs)
    16 -> V16 (take d16 vs)
    32 -> V32 (take d32 vs)
    _  -> vvs
  V1024 vs -> case newLengthC vs of
    2   -> V2 (take d2 vs)
    4   -> V4 (take d4 vs)
    8   -> V8 (take d8 vs)
    16  -> V16 (take d16 vs)
    32  -> V32 (take d32 vs)
    64  -> V64 (take d64 vs)
    128 -> V128 (take d128 vs)
    256 -> V256 (take d256 vs)
    512 -> V512 (take d512 vs)
    _   -> vvs
  V2048 vs -> case newLengthC vs of
    2    -> V2 (take d2 vs)
    4    -> V4 (take d4 vs)
    8    -> V8 (take d8 vs)
    16   -> V16 (take d16 vs)
    32   -> V32 (take d32 vs)
    64   -> V64 (take d64 vs)
    128  -> V128 (take d128 vs)
    256  -> V256 (take d256 vs)
    512  -> V512 (take d512 vs)
    1024 -> V1024 (take d1024 vs)
    _    -> vvs
  V4096 vs -> case newLengthC vs of
    2    -> V2 (take d2 vs)
    4    -> V4 (take d4 vs)
    8    -> V8 (take d8 vs)
    16   -> V16 (take d16 vs)
    32   -> V32 (take d32 vs)
    64   -> V64 (take d64 vs)
    128  -> V128 (take d128 vs)
    256  -> V256 (take d256 vs)
    512  -> V512 (take d512 vs)
    1024 -> V1024 (take d1024 vs)
    -- 2048 -> V2048 (take d2048 vs)
    _    -> vvs
  V8192 vs -> case newLengthC vs of
    2    -> V2 (take d2 vs)
    4    -> V4 (take d4 vs)
    8    -> V8 (take d8 vs)
    16   -> V16 (take d16 vs)
    32   -> V32 (take d32 vs)
    64   -> V64 (take d64 vs)
    128  -> V128 (take d128 vs)
    256  -> V256 (take d256 vs)
    512  -> V512 (take d512 vs)
    1024 -> V1024 (take d1024 vs)
    -- 2048 -> V2048 (take d2048 vs)
    -- 4096 -> V4096 (take d4096 vs)
    _    -> vvs
  V16383 vs -> case newLengthC vs of
    2    -> V2 (take d2 vs)
    4    -> V4 (take d4 vs)
    8    -> V8 (take d8 vs)
    16   -> V16 (take d16 vs)
    32   -> V32 (take d32 vs)
    64   -> V64 (take d64 vs)
    128  -> V128 (take d128 vs)
    256  -> V256 (take d256 vs)
    512  -> V512 (take d512 vs)
    1024 -> V1024 (take d1024 vs)
    -- 2048 -> V2048 (take d2048 vs)
    -- 4096 -> V4096 (take d4096 vs)
    -- 8192 -> V8192 (take d8192 vs)
    _    -> vvs
  V32768 vs -> case newLengthC vs of
    2    -> V2 (take d2 vs)
    4    -> V4 (take d4 vs)
    8    -> V8 (take d8 vs)
    16   -> V16 (take d16 vs)
    32   -> V32 (take d32 vs)
    64   -> V64 (take d64 vs)
    128  -> V128 (take d128 vs)
    256  -> V256 (take d256 vs)
    512  -> V512 (take d512 vs)
    1024 -> V1024 (take d1024 vs)
    -- 2048  -> V2048 (take d2048 vs)
    -- 4096  -> V4096 (take d4096 vs)
    -- 8192  -> V8192 (take d8192 vs)
    -- 16383 -> V16383 (take d16383 vs)
    _    -> vvs
  V65536 vs -> case newLengthC vs of
    2    -> V2 (take d2 vs)
    4    -> V4 (take d4 vs)
    8    -> V8 (take d8 vs)
    16   -> V16 (take d16 vs)
    32   -> V32 (take d32 vs)
    64   -> V64 (take d64 vs)
    128  -> V128 (take d128 vs)
    256  -> V256 (take d256 vs)
    512  -> V512 (take d512 vs)
    1024 -> V1024 (take d1024 vs)
    -- 2048  -> V2048 (take d2048 vs)
    -- 4096  -> V4096 (take d4096 vs)
    -- 8192  -> V8192 (take d8192 vs)
    -- 16383 -> V16383 (take d16383 vs)
    -- 32768 -> V32768 (take d32768 vs)
    _    -> vvs
  _ -> vvs
































