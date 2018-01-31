{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}

module NonHW where

import qualified Data.Char as C (digitToInt)
import qualified Data.List as L (drop, foldl, head, last, length, repeat, reverse, take)
import qualified Prelude as P (replicate, (++), readFile, splitAt)

import CLaSH.Prelude
import Control.Monad (ap, liftM, void)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.String ()
import Interpreter
import Parser

-- | Convert vs to string dropping the '~' tail characters
vecToString :: V -> String
vecToString vs = do
  let removeTilda = foldl (\acc c -> if c /= '~' then c : acc else acc) ""
      result v = show $ L.reverse (removeTilda v)
  case vs of
    V1024 v -> result v
    V512  v -> result v
    V256  v -> result v
    V128  v -> result v
    V64   v -> result v
    V32   v -> result v
    V16   v -> result v
    V8    v -> result v
    V4    v -> result v
    V2    v -> result v

-- | Pretty print a (show (Vec n Char))
showVec :: String -> String
showVec s = if L.length s > 1 && L.head s == '<' && L.last s == '>'
  then filter (\c -> c /= '\'' && c /= '~') $ show s
  else case L.take 4 s of
    "Sym " -> "Sym " P.++ showVec (L.drop 4 s)
    "Str " -> "Str " P.++ showVec (L.drop 4 s)
    "Q2 <" -> "Q2 " P.++ showVec (L.drop 3 s) P.++ ", "
    "Q4 <" -> "Q4 " P.++ showVec (L.drop 3 s) P.++ ", "
    "Q8 <" -> "Q8 " P.++ showVec (L.drop 3 s) P.++ ", "
    "Q16 " -> "Q16 " P.++ showVec (L.drop 4 s) P.++ ", "
    "Q32 " -> "Q32 " P.++ showVec (L.drop 4 s) P.++ ", "
    "Q64 " -> "Q64 " P.++ showVec (L.drop 4 s) P.++ ", "
    "Q128" -> "Q128 " P.++ showVec (L.drop 5 s) P.++ ", "
    "Q256" -> "Q256 " P.++ showVec (L.drop 5 s) P.++ ", "
    "Q512" -> "Q512 " P.++ showVec (L.drop 5 s) P.++ ", "
    "1024" -> "Q1024 " P.++ showVec (L.drop 6 s) P.++ ", "
    _      -> s

-- | Petty print a parse result
showParse :: Show a => Maybe (a, V) -> String
showParse res = if isJust res
  then do
    let (r, vec) = fromJust res
        r'       = showVec $ show r
    "(" P.++ r' P.++ ", " P.++ vecToString vec P.++ ")"
  else "Nothing"

-- | Covert s to (Vec n Char) where n < 65
loadStr64 :: (m + n) ~ 64 => SNat n -> String -> Vec n Char
loadStr64 n s = go s' $ drop (subSNat d64 n) blank64
 where
  s' = padStrN (fromIntegral (snatToInteger n) :: Int) s
  go ""     vs = vs
  go (c:cs) vs = go cs (vs <<+ c)

-- | Covert s to (Vec n Char) where n < 1024
loadStr1024 n s = go s' $ drop (subSNat d1024 n) blank1024
 where
  s' = padStrN (fromIntegral (snatToInteger n) :: Int) s
  go ""     vs = vs
  go (c:cs) vs = go cs (vs <<+ c)


-- | Covert s a pruned V
loadStr :: String -> V
loadStr s = pruneV $ V65536 (go s' blank65536)
 where
  s' = L.reverse s
  go ""     vs = vs
  go (c:cs) vs = go cs (c +>> vs)


padStrN :: Int -> String -> String
padStrN n s = s P.++ P.replicate (n - L.length s) '~'

replaceStr :: String -> String -> String -> String
replaceStr _   _   []  = []
replaceStr old new str = go str
 where
  go [] = []
  go str'@(x:xs) =
    let (prefix, rest) = P.splitAt n str'
    in  if old == prefix then new P.++ go rest else x : go xs
  n = L.length old

longV :: Vec 16384 Char
longV = $(listToVecTH longSrc) ++ (repeat '~' :: Vec 3272 Char)
