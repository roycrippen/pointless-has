{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}

-- stack exec --resolver=nightly-2017-08-15 -- clash --interactive pointless/test/Spec.hs

module Main where

import qualified Data.Char as C (digitToInt)
import qualified Data.List as L (drop, foldl, head, last, length, repeat, reverse, take)
import qualified Prelude as P (replicate, (++))

import Clash.Prelude
import Control.Monad (ap, liftM, void)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.String ()
import Interpreter (Q (..), V (..), ValueP (..), lengthElem, pruneQ, pruneV)
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
    "Sym'" -> "Sym' " P.++ showVec (L.drop 5 s)
    "Str'" -> "Str' " P.++ showVec (L.drop 5 s)
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

-- xs' = parse nakedQuotations  (loadStr "[10 20 +] dup exec swap exec + [10 20 +] dup exec swap exec +")
-- ss' = showParse xs'

-- -- | Parser tests.
-- -- |

xs :: Vec 4 Char
xs = '1' :> '2' :> '3' :> '~' :> Nil

p001Src :: V
p001Src =
  loadStr
    "\"p001\" [ 1 n from-to-list [ [3 is-div-by] [5 is-div-by] cleave or ] filter sum ] define"

main :: IO ()
main = do
  putStrLn "Pointless in Clash tests\n"
  putStrLn ""
  parserTests

parserTests :: IO ()
parserTests = do
  putStrLn "parserTests..."

  let s1 = parse (oneOf $ loadStr64 d16 "cba") (loadStr "abc 123")
      r1 = "('a', \"bc 123\")"
  putStr $ "parse oneOf:             " P.++ show (r1 == showParse s1)
  putStrLn $ ",  result = " P.++ showParse s1

  let s2 = parse (string $ loadStr64 d16 "abc") (loadStr "abc   123")
      r2 = "(\"<a,b,c,,,,,,,,,,,,,>\", \"   123\")"
  putStr $ "parse string:            " P.++ show (r2 == showParse s2)
  putStrLn $ ",  result = " P.++ showParse s2

  let s3 = parse spaces (loadStr "   123")
      r3 = "((), \"123\")"
  putStr $ "parse spaces:            " P.++ show (r3 == showParse s3)
  putStrLn $ ",  result = " P.++ showParse s3

  let s4 = parse digit (loadStr "123")
      r4 = "('1', \"23\")"
  putStr $ "parse digit:             " P.++ show (r4 == showParse s4)
  putStrLn $ ",  result = " P.++ showParse s4

  let s5 = parse numberInt (loadStr "123")
      r5 = "(123, \"\")"
  putStr $ "parse numberInt:         " P.++ show (r5 == showParse s5)
  putStrLn $ ",  result = " P.++ showParse s5

  let s6 = parse numberInt (loadStr "123 abc")
      r6 = "(123, \" abc\")"
  putStr $ "parse numberInt:         " P.++ show (r6 == showParse s6)
  putStrLn $ ",  result = " P.++ showParse s6

  let s7 = parse (oneOf $ loadStr64 d16 "defa") (loadStr "abc   123")
      r7 = "('a', \"bc   123\")"
  putStr $ "parse oneOf:             " P.++ show (r7 == showParse s7)
  putStrLn $ ",  result = " P.++ showParse s7

  let s8 = parse (oneOf $ loadStr64 d16 "cdef") (loadStr "abc   123")
      r8 = "Nothing"
  putStr $ "parse oneOf:             " P.++ show (r8 == showParse s8)
  putStrLn $ ",  result = " P.++ showParse s8

  let s9 = parse (noneOf $ loadStr64 d16 "def") (loadStr "abc   123")
      r9 = "('a', \"bc   123\")"
  putStr $ "parse NoneOf:            " P.++ show (r9 == showParse s9)
  putStrLn $ ",  result = " P.++ showParse s9

  let s10 = parse (manyChar (char 'a')) (loadStr "aaa bbb")
      r10 = "(V4 <'a','a','a','~'>, \" bbb\")"
  putStr $ "parse manyChar:          " P.++ show (r10 == showParse s10)
  putStrLn $ ",  result = " P.++ showParse s10

  let s11 = parse (manyChar (char 'a')) (loadStr "a bbb")
      r11 = "(V2 <'a','~'>, \" bbb\")"
  putStr $ "parse manyChar:          " P.++ show (r11 == showParse s11)
  putStrLn $ ",  result = " P.++ showParse s11

  let s12 = parse (manyChar (char 'a')) (loadStr "bbb aaa")
      r12 = "(V2 <'~','~'>, \"bbb aaa\")"
  putStr $ "parse manyChar:          " P.++ show (r12 == showParse s12)
  putStrLn $ ",  result = " P.++ showParse s12

  let s13 = parse (many1Char (char 'a')) (loadStr "aaa bbb")
      r13 = "(V4 <'a','a','a','~'>, \" bbb\")"
  putStr $ "parse many1Char:         " P.++ show (r13 == showParse s13)
  putStrLn $ ",  result = " P.++ showParse s13

  let s14 = parse (many1Char (char 'a')) (loadStr "a bbb")
      r14 = "(V2 <'a','~'>, \" bbb\")"
  putStr $ "parse many1Char:         " P.++ show (r14 == showParse s14)
  putStrLn $ ",  result = " P.++ showParse s14

  let s15 = parse (many1Char (char 'a')) (loadStr "bbb aaa")
      r15 = "Nothing"
  putStr $ "parse many1Char:         " P.++ show (r15 == showParse s15)
  putStrLn $ ",  result = " P.++ showParse s15

  let s16 = parse (manyTillChar anyChar (char '}')) (loadStr "123 ccc")
      r16 = "(V8 <'1','2','3',' ','c','c','c','~'>, \"\")"
  putStr $ "parse manyTillChar:      " P.++ show (r16 == showParse s16)
  putStrLn $ ",  result = " P.++ showParse s16

  let s17 = parse (manyTillChar anyChar (char '}')) (loadStr "123} ccc")
      r17 = "(V4 <'1','2','3','~'>, \"} ccc\")"
  putStr $ "parse manyTillChar:      " P.++ show (r17 == showParse s17)
  putStrLn $ ",  result = " P.++ showParse s17

  let
    s18 = parse quotedString (loadStr "\"hello world\" 123")
    r18
      = "(V16 <'h','e','l','l','o',' ','w','o','r','l','d','~','~','~','~','~'>, \" 123\")"
  putStr $ "parse quotedString:      " P.++ show (r18 == showParse s18)
  putStrLn $ ",  result = " P.++ showParse s18

  let s19 = parse firstLetter (loadStr "abc")
      r19 = "('a', \"bc\")"
  putStr $ "parse firstLetter:       " P.++ show (r19 == showParse s19)
  putStrLn $ ",  result = " P.++ showParse s19

  let s20 = parse firstLetter (loadStr "_abc")
      r20 = "('_', \"abc\")"
  putStr $ "parse firstLetter:       " P.++ show (r20 == showParse s20)
  putStrLn $ ",  result = " P.++ showParse s20

  let s21 = parse charP (loadStr "'z' abc")
      r21 = "(Chr' 'z', \" abc\")"
  putStr $ "parse charP:             " P.++ show (r21 == showParse s21)
  putStrLn $ ",  result = " P.++ showParse s21

  let s22 = parse charP (loadStr "abc")
      r22 = "Nothing"
  putStr $ "parse charP:             " P.++ show (r22 == showParse s22)
  putStrLn $ ",  result = " P.++ showParse s22

  let s23 = parse numberP (loadStr "123 abc")
      r23 = "(NumP 123, \" abc\")"
  putStr $ "parse numberP:           " P.++ show (r23 == showParse s23)
  putStrLn $ ",  result = " P.++ showParse s23

  let s24 = parse numberP (loadStr "-123 abc")
      r24 = "(NumP (-123), \" abc\")"
  putStr $ "parse numberP:           " P.++ show (r24 == showParse s24)
  putStrLn $ ",  result = " P.++ showParse s24

  let s25 = parse numberP (loadStr "abc")
      r25 = "Nothing"
  putStr $ "parse numberP:           " P.++ show (r25 == showParse s25)
  putStrLn $ ",  result = " P.++ showParse s25

  let s26 = parse quotedStringP (loadStr "\"abc\" 123")
      r26 = "(Str' (V4 <'a','b','c','~'>), \" 123\")"
  putStr $ "parse quotedStringP:     " P.++ show (r26 == showParse s26)
  putStrLn $ ",  result = " P.++ showParse s26

  let s27 = parse word (loadStr "dup +")
      r27 = "(Sym' \"<d,u,p,,,,,,,,,,,,,>\", \" +\")"
  putStr $ "parse word:              " P.++ show (r27 == showParse s27)
  putStrLn $ ",  result = " P.++ showParse s27

  let s28 = parse lineComment (loadStr "$ zzz \n dup")
      r28 = "((), \"dup\")"
  putStr $ "parse lineComment:       " P.++ show (r28 == showParse s28)
  putStrLn $ ",  result = " P.++ showParse s28

  let s29 = parse blockComment (loadStr "{ zzz } dup")
      r29 = "((), \"dup\")"
  putStr $ "parse blockComment:      " P.++ show (r29 == showParse s29)
  putStrLn $ ",  result = " P.++ showParse s29

  let s30 = parse comment (loadStr "$ zzz \n dup")
      r30 = "((), \"dup\")"
  putStr $ "parse comment:           " P.++ show (r30 == showParse s30)
  putStrLn $ ",  result = " P.++ showParse s30

  let s31 = parse comment (loadStr "{ zzz } dup")
      r31 = "((), \"dup\")"
  putStr $ "parse comment:           " P.++ show (r31 == showParse s31)
  putStrLn $ ",  result = " P.++ showParse s31

  let s32 = parse comments (loadStr "$ a \n {z} {z} dup")
      r32 = "((), \"dup\")"
  putStr $ "parse comments:          " P.++ show (r32 == showParse s32)
  putStrLn $ ",  result = " P.++ showParse s32

  let s33 = parse specification (loadStr "( X -> -- ) a")
      r33 = "((), \"a\")"
  putStr $ "parse specification:     " P.++ show (r33 == showParse s33)
  putStrLn $ ",  result = " P.++ showParse s33

  let s34 = parse specifications (loadStr "(a) (b) a")
      r34 = "((), \"a\")"
  putStr $ "parse specifications:    " P.++ show (r34 == showParse s34)
  putStrLn $ ",  result = " P.++ showParse s34

  let s35 = parse spacesCommentsSpecifications (loadStr " {a} (b) a")
      r35 = "((), \"a\")"
  putStr $ "parse spacesCommentsSpecifications: " P.++ show
    (r35 == showParse s35)
  putStrLn $ ",  result = " P.++ showParse s35

  let s36        = parse nakedQuotations p001Src
      (Q4 vs, _) = fromJust s36
  putStr $ "parse nakedQuotations:    " P.++ show (length vs == 4)
  putStrLn $ ",  result = " P.++ showParse s36











