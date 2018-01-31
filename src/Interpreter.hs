{-# LANGUAGE DataKinds           #-}

module Interpreter where

import           CLaSH.Prelude
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

data ValueP = Sym (Vec 16 Char)
             | NumP Int
             | Chr Char
             | Str V
             | Quot Q
             | EmptyQ
             deriving (Eq, Ord, Show)

d2048 = mulSNat d2 d1024
d4096 = mulSNat d2 d2048
d8192 = mulSNat d2 d4096
d16384 = mulSNat d2 d8192
d32768 = mulSNat d2 d16384
d65536 = mulSNat d2 d32768

data Q = Q2     (Vec 2     ValueP)
       | Q4     (Vec 4     ValueP)
       | Q8     (Vec 8     ValueP)
       | Q16    (Vec 16    ValueP)
       | Q32    (Vec 32    ValueP)
       | Q64    (Vec 64    ValueP)
       | Q128   (Vec 128   ValueP)
       | Q256   (Vec 256   ValueP)
       | Q512   (Vec 512   ValueP)
       | Q1024  (Vec 1024  ValueP)
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
       | V16384 (Vec 16384 Char)
       | V32768 (Vec 32768 Char)
       | V65536 (Vec 65536 Char)
       deriving (Eq, Ord, Show)


-- data Lang = Lang { vocab   :: Vocabulary
--                  , stack   :: [ValueP]
--                  , result  :: [String]
--                  , display :: String
--                  , mode    :: Mode
--                  }
--                  deriving (Show)

-- data WordP = Quotation [ValueP] | Function (Lang -> Lang)
-- instance Show WordP where show = formatWordP

-- data Mode = REPL | WEBSOCKET
-- instance Show Mode where show = formatMode

-- type Vocabulary = M.Map String WordP

-- getWord :: String -> Vocabulary -> Maybe WordP
-- getWord = M.lookup

-- isTrue :: ValueP -> Bool
-- isTrue (NumP x) = x /= 0
-- isTrue (Quot q) = not (null q)
-- isTrue _        = False

-- toTruth :: Bool -> ValueP
-- toTruth b = if b then NumP 1 else NumP 0

-- runWord :: WordP -> Lang -> Lang
-- runWord w lang = case w of
--   Quotation q -> runQuotation q lang
--   Function  f -> f lang

-- runQuotation :: [ValueP] -> Lang -> Lang
-- runQuotation quotation lang = case quotation of
--   []     -> lang
--   (i:is) -> runQuotation is (runInstruction i lang)

-- runInstruction :: ValueP -> Lang -> Lang
-- runInstruction ins lang = case ins of
--   Sym w -> case getWord w (vocab lang) of
--     Just w' -> runWord w' lang
--     Nothing -> lang { result = msg : result lang }
--       where msg = "ERROR(getWord): not a valid word " P.++ show w
--   x -> lang { stack = x : stack lang }

-- -- |
-- -- | pretty printers
-- -- |

-- formatV :: ValueP -> String
-- formatV (Sym  s ) = s
-- -- formatV (NumP   n) = if isInteger n
-- --   then show (truncate n :: Integer)
-- --   else floatStr
-- --   where floatStr = showFFloat (Just 6) n ""
-- formatV (NumP n ) = show n
-- formatV (Quot []) = "[]"
-- formatV (Quot q ) = L.concat ["[ ", unwords $ L.map formatV q, " ]"]
-- formatV (Chr  c ) = [c]
-- formatV (Str  s ) = show s

-- formatPutch :: ValueP -> Maybe Char
-- -- formatPutch (NumP n) = if isInteger n then Just charFromInt else Nothing
-- -- where charFromInt = chr (truncate n :: Int)
-- formatPutch (NumP n) = Just $ chr n
-- formatPutch (Chr  c) = Just c
-- formatPutch _        = Nothing

-- formatWordP :: WordP -> String
-- formatWordP (Quotation xs) = formatV (Quot xs)
-- formatWordP (Function  _ ) = "Primitive function"

-- formatWordAST :: WordP -> String
-- formatWordAST (Quotation xs) = show xs
-- formatWordAST (Function  _ ) = "function: Vocabulary -> [ValueP] -> [ValueP]"

-- formatStack :: [ValueP] -> [String]
-- formatStack = L.map formatV

-- -- isInteger :: Double -> Bool
-- -- isInteger d = abs realFrac < 0.0000001
-- --  where
-- --   (_, realFrac) = properFraction' d
-- --   properFraction' :: Double -> (Integer, Double)
-- --   properFraction' = properFraction

-- formatMode :: Mode -> String
-- formatMode REPL      = "REPL mode"
-- formatMode WEBSOCKET = "Websocket mode"

-- replaceStr :: String -> String -> String -> String
-- replaceStr _   _   []  = []
-- replaceStr old new str = go str
--  where
--   go [] = []
--   go str'@(x:xs) =
--     let (prefix, rest) = L.splitAt n str'
--     in  if old == prefix then new P.++ go rest else x : go xs
--   n = L.length old


-- | Helper functions.
-- |
-- | Calculate nearest power of 2 length >= n ofr Vec of ValueP.
newLengthVP :: KnownNat n => Vec n ValueP -> Int
newLengthVP vs = case lengthElem EmptyQ vs of
  0 -> 2
  1 -> 2
  _ -> 2 ^ ceiling (logBase 2 $ fromIntegral (lengthElem EmptyQ vs)) :: Int

-- | Calculate nearest power of 2 length >= n ofr Vec of Char.
newLengthC :: KnownNat n => Vec n Char -> Int
newLengthC vs = case lengthElem '~' vs of
  0 -> 2
  1 -> 2
  _ -> 2 ^ ceiling (logBase 2 $ fromIntegral (lengthElem '~' vs)) :: Int

-- | Count non '~' consecutive charaters starting a Vector.
lengthElem :: (Eq a, KnownNat n) => a -> Vec n a -> Int
lengthElem a vs = case CLaSH.Prelude.findIndex (==a) vs of
  Just n -> fromIntegral (toInteger n)
  _      -> CLaSH.Prelude.length vs

-- | Shorten type Q vector as much as possible.
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

-- | Shorten type V vector of Q as much as possible.
pruneV :: V -> V
pruneV vvs = case vvs of
  --
  V4 vs -> case newLengthC vs of
    2 -> V2 (take d2 vs)
    _ -> vvs
  V8 vs -> case newLengthC vs of
    2 -> V2 (take d2 vs)
    4 -> V4 (take d4 vs)
    _ -> vvs
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
  --  
  V128 vs -> case newLengthC vs of
    2  -> V2 (take d2 vs)
    4  -> V4 (take d4 vs)
    8  -> V8 (take d8 vs)
    16 -> V16 (take d16 vs)
    32 -> V32 (take d32 vs)
    64 -> V64 (take d64 vs)
    _  -> vvs
  --  
  V256 vs -> case newLengthC vs of
    2   -> V2 (take d2 vs)
    4   -> V4 (take d4 vs)
    8   -> V8 (take d8 vs)
    16  -> V16 (take d16 vs)
    32  -> V32 (take d32 vs)
    64  -> V64 (take d64 vs)
    128 -> V128 (take d128 vs)
    _   -> vvs
  --  
  V512 vs -> case newLengthC vs of
    2   -> V2 (take d2 vs)
    4   -> V4 (take d4 vs)
    8   -> V8 (take d8 vs)
    16  -> V16 (take d16 vs)
    32  -> V32 (take d32 vs)
    64  -> V64 (take d64 vs)
    128 -> V128 (take d128 vs)
    256 -> V256 (take d256 vs)
    _   -> vvs
  --  
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
    2048 -> V2048 (take d2048 vs)
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
    2048 -> V2048 (take d2048 vs)
    4096 -> V4096 (take d4096 vs)
    _    -> vvs
  V16384 vs -> case newLengthC vs of
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
    2048 -> V2048 (take d2048 vs)
    4096 -> V4096 (take d4096 vs)
    8192 -> V8192 (take d8192 vs)
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
    2048  -> V2048 (take d2048 vs)
    4096  -> V4096 (take d4096 vs)
    8192  -> V8192 (take d8192 vs)
    16384 -> V16384 (take d16384 vs)
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
    2048  -> V2048 (take d2048 vs)
    4096  -> V4096 (take d4096 vs)
    8192  -> V8192 (take d8192 vs)
    16384 -> V16384 (take d16384 vs)
    32768 -> V32768 (take d32768 vs)
    _    -> vvs
  _ -> vvs




longSrc :: String
longSrc
  = "        \"assert'\"   \
\     [ rollup    \
\       [=]    \
\       [ \"OK: \" ]    \
\       [ \"****************** ERROR: \"]    \
\       ifte    \
\       rollup pop2 swap concat   \
\     ] define   \
\      \
\   [ \"t001\" [ 10 10 * [190 >] [] [0] ifte                          0          \"t001 ifte\"            assert' ]    \ 
\     \"t002\" [ 10 10 * dup [ 90 >] [] [0] ifte                      100        \"t002 ifte\"            assert' ]    \ 
\     \"t003\" [ 10 [20 >] [] [0] ifte                                0          \"t003 ifte\"            assert' ]    \ 
\     \"t004\" [ 10 20 [>] [1] [0] ifte                               0          \"t004 ifte\"            assert' ]    \ 
\     \"t005\" [ [10 20 <] [1] [0] ifte                               1          \"t005 ifte\"            assert' ]    \ 
\     \"t006\" [ 10 dup [20 <] [] [0] ifte                            10         \"t006 ifte\"            assert' ]    \ 
\     \"t007\" [ [3 2 1] [10 *] step pop pop                          30         \"t007 step\"            assert' ]    \ 
\     \"t008\" [ 10 [3 >] [20 <] sequor                               true       \"t008 sequor\"          assert' ]    \ 
\     \"t009\" [ 10 [3 <] [20 >] sequor                               false      \"t009 sequor\"          assert' ]    \ 
\     \"t010\" [ 10 [3 <] [20 <] sequor                               true       \"t010 sequor\"          assert' ]    \ 
\     \"t011\" [ 10 [2 3] cons  sum                                   15         \"t011 cons\"            assert' ]    \ 
\     \"t012\" [ [2 3] 10 swons sum                                   15         \"t012 swons\"           assert' ]    \ 
\     \"t013\" [ 1 2 3 4 5 [\"abc\"] unstack                        \"abc\"      \"t013 unstack\"         assert' ]    \ 
\     \"t014\" [ 1 2 3 4 5 newstack 1                                 1          \"t014 newstack\"        assert' ]    \ 
\     \"t015\" [ [1 2 3] reverse first                                3          \"t015 reverse\"         assert' ]    \ 
\     \"t016\" [ \"abc\" reverse reverse                            \"abc\"      \"t016 reverse\"         assert' ]    \ 
\     \"t017\" [ [10 10 * 10 *] exec                                  1000       \"t017 exec\"            assert' ]    \ 
\     \"t018\" [ [1 1 + 1 +] exec                                     3          \"t018 exec\"            assert' ]    \ 
\     \"t019\" [ [1 1 +] eval swap size +                             5          \"t019 eval\"            assert' ]    \ 
\     \"t020\" [ [1 2 3] [[1 +] map] [] iflist sum                    9          \"t020 iflist\"          assert' ]    \ 
\     \"t021\" [ [1 2 3] uncons pop                                   1          \"t021 uncons\"          assert' ]    \ 
\     \"t022\" [ [1 2 3] unswons                                      1          \"t022 unswons\"         assert' ]    \ 
\     \"t023\" [ 'b' \"abbbcbbcd\" in                                 true       \"t023 in\"              assert' ]    \ 
\     \"t024\" [ 'd' \"abbbcbbcd\" in                                 true       \"t024 in\"              assert' ]    \ 
\     \"t025\" [ 'z' \"abbbcbbcd\" in                                 false      \"t025 in\"              assert' ]    \ 
\     \"t026\" [ 2 [1 2 3 4 5] in                                     true       \"t026 in\"              assert' ]    \ 
\     \"t027\" [ 6 [1 2 3 4 5] in                                     false      \"t027 in\"              assert' ]    \ 
\     \"t028\" [ [ [1 2 3] [1 +] map ] exec sum                       9          \"t028 exec\"            assert' ]    \ 
\     \"t029\" [ newstack [2 2 +] [true] [1 1 +] [] ifte stack size   2          \"t029 ifte\"            assert' ]    \ 
\     \"t030\" [ \"abc\" [to-upper] map                             \"ABC\"      \"t030 to-upper\"        assert' ]    \ 
\     \"t031\" [ \"ABC\" [to-lower] map                             \"abc\"      \"t031 to-lower\"        assert' ]    \ 
\     \"t032\" [ [\"aaa\" \"bbb\"] \"\" [concat] fold               \"aaabbb\"   \"t032 fold/concat\"     assert' ]    \ 
\     \"t033\" [ 5 fact                                               120        \"t033 fact\"            assert' ]    \ 
\     \"t034\" [ 'a' 'f' \"\" from-to                               \"abcdef\"   \"t034 from-to\"         assert' ]    \ 
\     \"t035\" [ 25 fib                                               75025      \"t035 fib\"             assert' ]    \ 
\     \"t036\" [ newstack 1 2 3 4 5 6 7 8 9 [+ + +] unary stack size  9          \"t036 unary\"           assert' ]    \                      
\     \"t037\" [ newstack 1 2 3 4 5 6 7 8 9 [+ + +] unary             30         \"t037 unary\"           assert' ]    \                      
\     \"t038\" [ newstack 1 2 3 4 5 6 7 8 9 [+ + +] unary stack sum   66         \"t038 unary\"           assert' ]    \                      
\     \"t039\" [ newstack 1 2 3 4 5 [+ + +] nullary stack size        6          \"t039 nullary\"         assert' ]    \                      
\     \"t040\" [ newstack 1 2 3 4 5 [+ + +] nullary                   14         \"t040 nullary\"         assert' ]    \                      
\     \"t041\" [ newstack 1 2 3 4 5 [+ + +] nullary stack sum         29         \"t041 nullary\"         assert' ]    \                      
\     \"t042\" [ newstack 1 2 3 4 5 [+ + +] unary2 stack size         5          \"t042 unary2\"          assert' ]    \                      
\     \"t043\" [ newstack 1 2 3 4 5 [+ + +] unary2                    20         \"t043 unary2\"          assert' ]    \                      
\     \"t044\" [ newstack 1 2 3 4 5 [+ + +] unary2 stack sum          36         \"t044 unary2\"          assert' ]    \                      
\     \"t045\" [ newstack 3 [9 1 +] times stack size                  3          \"t045 times\"           assert' ]    \                      
\     \"t046\" [ newstack 3 [9 1 +] times                             10         \"t046 times\"           assert' ]    \                      
\     \"t047\" [ newstack 3 [9 1 +] times stack sum                   30         \"t047 times\"           assert' ]    \                      
\     \"t048\" [ [] null                                              true       \"t048 null\"            assert' ]    \ 
\     \"t049\" [ [2] null                                             false      \"t049 null\"            assert' ]    \ 
\     \"t050\" [ 0 null                                               true       \"t050 null\"            assert' ]    \ 
\     \"t051\" [ 2 null                                               false      \"t051 null\"            assert' ]    \ 
\     \"t052\" [ 0 positive                                           false      \"t052 positive\"        assert' ]    \ 
\     \"t053\" [ 1 positive                                           true       \"t053 positive\"        assert' ]    \ 
\     \"t054\" [ -1 positive                                          false      \"t054 positive\"        assert' ]    \ 
\     \"t055\" [ 0 negative                                           false      \"t055 negative\"        assert' ]    \ 
\     \"t056\" [ 1 negative                                           false      \"t056 negative\"        assert' ]    \ 
\     \"t057\" [ -1 negative                                          true       \"t057 negative\"        assert' ]    \ 
\     \"t058\" [ 1 even                                               false      \"t058 even\"            assert' ]    \ 
\     \"t059\" [ 2 even                                               true       \"t059 even\"            assert' ]    \ 
\     \"t060\" [ 2 odd                                                false      \"t060 odd\"             assert' ]    \ 
\     \"t061\" [ 1 odd                                                true       \"t061 odd\"             assert' ]    \ 
\     \"t062\" [ [1 2 3 4 5] second                                   2          \"t062 second\"          assert' ]    \ 
\     \"t063\" [ \"abc\" third                                        'c'        \"t063 third\"           assert' ]    \ 
\     \"t064\" [ [1 2 3 4 5] fourth                                   4          \"t064 fourth\"          assert' ]    \ 
\     \"t065\" [ \"abcdef\" fifth                                     'e'        \"t065 fifth\"           assert' ]    \ 
\     \"t066\" [ [1 2] [10 20] zip unstack sum                        11         \"t066 linrec/zip\"      assert' ]    \ 
\     \"t067\" [ [1 2] [10 20] zip unstack pop sum                    22         \"t067 linrec/zip\"      assert' ]    \ 
\     \"t068\" [ 5 [null] [succ] [dup pred] [*] linrec                120        \"t068 linrec\"          assert' ]    \ 
\     \"t069\" [ 1 5 from-to-list sum                                 15         \"t069 from-to-list\"    assert' ]    \ 
\     \"t070\" [ 1 5 from-to-list sum                                 15         \"t070 from-to-list\"    assert' ]    \ 
\     \"t071\" [ 1 10 from-to-list [even] filter sum                  30         \"t071 filter/even\"     assert' ]    \ 
\     \"t072\" [ 1 10 from-to-list [odd] filter sum                   25         \"t072 filter/odd\"      assert' ]    \ 
\     \"t073\" [ 1 10 from-to-list [even] split sum                   25         \"t073 split\"           assert' ]    \ 
\     \"t074\" [ 1 10 from-to-list [even] split swap sum              30         \"t074 split\"           assert' ]    \ 
\     \"t075\" [ \"abcde\" ['c' >] split swap                       \"de\"       \"t075 split\"           assert' ]    \ 
\     \"t076\" [ \"abcde\" ['c' >] split                            \"abc\"      \"t076 split\"           assert' ]    \ 
\     \"t077\" [ 'a' succ succ succ                                   'd'        \"t077 succ\"            assert' ]    \ 
\     \"t078\" [ 'd' pred pred pred                                   'a'        \"t078 pred\"            assert' ]    \ 
\     \"t079\" [ \"abc\" [to-upper] mapr                            \"ABC\"      \"t079 mapr\"            assert' ]    \ 
\     \"t080\" [ [1 2 3] [10 *] mapr sum                              60         \"t080 mapr\"            assert' ]    \ 
\     \"t081\" [ [1 2 3 4 5] 0 [+] foldr                              15         \"t081 foldr\"           assert' ]    \ 
\     \"t082\" [ [\"aaa\" \"bbb\"] \"\" [concat] foldr              \"aaabbb\"   \"t082 foldr\"           assert' ]    \ 
\     \"t083\" [ [10 11] [2 3] [*] stepr2                             33         \"t083 stepr2\"          assert' ]    \ 
\     \"t084\" [ [10 11] [2 3] [*] stepr2 pop                         20         \"t084 stepr2\"          assert' ]    \ 
\     \"t085\" [ [1 2] [2 3] [+] mapr2 first                          3          \"t085 mapr2\"           assert' ]    \ 
\     \"t086\" [ [1 2] [2 3] [+] mapr2 second                         5          \"t086 mapr2\"           assert' ]    \ 
\     \"t087\" [ [1 2] [4 5]  interleave2list first                   1          \"t087 interleave2list\" assert' ]    \ 
\     \"t088\" [ [1 2] [4 5]  interleave2list second                  4          \"t088 interleave2list\" assert' ]    \ 
\     \"t089\" [ [1 2] [4 5]  interleave2list third                   2          \"t089 interleave2list\" assert' ]    \ 
\     \"t090\" [ [1 2] [4 5]  interleave2list fourth                  5          \"t090 interleave2list\" assert' ]    \ 
\     \"t091\" [ [3 9 18] sum                                         30         \"t091 sum\"             assert' ]    \ 
\     \"t092\" [ 1 [10 <] [1 +] while                                 10         \"t092 while\"           assert' ]    \ 
\     \"t093\" [ [1 2 3 4 5] [rest null] [first] [rest] tailrec       5          \"t093 tailrec\"         assert' ]    \ 
\     \"t094\" [ [1 2 3] [4 5] concat sum                             15         \"t094 concat\"          assert' ]    \ 
\     \"t095\" [ \"abc\" \"de\" concat                              \"abcde\"    \"t095 concat\"          assert' ]    \ 
\     \"t096\" [ [1 2 3] [4 5] swoncat sum                            15         \"t096 swoncat\"         assert' ]    \ 
\     \"t097\" [ \"abc\" \"de\" swoncat                             \"deabc\"    \"t097 swoncat\"         assert' ]    \ 
\     \"t098\" [ 2 10 [1 +] dip                                       10         \"t098 dip\"             assert' ]    \ 
\     \"t099\" [ 2 10 [1 +] dip pop                                   3          \"t099 dip\"             assert' ]    \ 
\     \"t100\" [ newstack 1 2 3 [4 5 +] eval stack size               5          \"t100 eval\"            assert' ]    \ 
\     \"t101\" [ newstack 1 2 3 [4 5 +] eval pop size                 3          \"t101 eval\"            assert' ]    \ 
\     \"t102\" [ newstack 1 2 3 [4 5 +] exec stack sum                15         \"t102 exec\"            assert' ]    \ 
\     \"t103\" [ newstack 1 2 3 [4 +] exec stack sum                  10         \"t103 exec\"            assert' ]    \ 
\     \"t104\" [ 646 874 gcd                                          38         \"t104 gcd\"             assert' ]    \ 
\     \"t105\" [ 1000033 prime                                        1          \"t105 prime\"           assert' ]    \
\     \"t106\" [ 1000034 prime                                        0          \"t106 prime\"           assert' ]    \
\     \"t107\" [ 25 \"roy\" set-var roy                               25         \"t107 set-var\"         assert' ]    \
\   ] defines   \
\      \
\   \"test-list\"   \
\   [ [ t001 t002 t003 t004 t005 t006 t007 t008 t009 t010 t011 t012 t013 t014 t015 t016 t017 t018 t019 t020    \
\       t021 t022 t023 t024 t025 t026 t027 t028 t029 t030 t031 t032 t033 t034 t035 t036 t037 t038 t039 t040    \
\       t041 t042 t043 t044 t045 t046 t047 t048 t049 t050 t051 t052 t053 t054 t055 t056 t057 t058 t059 t060      \
\       t061 t062 t063 t064 t065 t066 t067 t068 t069 t070 t071 t072 t073 t074 t075 t076 t077 t078 t079 t080    \
\       t081 t082 t083 t084 t085 t086 t087 t088 t089 t090 t091 t092 t093 t094 t095 t096 t097 t098 t099 t100    \
\       t101 t102 t103 t104 t105 t106 t107   \
\     ]   \
\   ] define   \
\   \
\   \"run-all-tests\"  [ test-list [] [ [] cons exec tx newstack ] fold ] define    \
\      \
\   \"running test...\n\" putchars tx   \
\   \
\run-all-tests tx"




















































