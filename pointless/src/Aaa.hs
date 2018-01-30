{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
-- {-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- stack exec --resolver=nightly-2017-08-15 -- clash --interactive pointless/src/Aaa.hs

module Aaa where

import           Clash.Prelude         hiding ((<|>))
import           Clash.Promoted.Nat.TH

import           Control.Monad         (ap, liftM, void)
import qualified Data.Char             as C (digitToInt)
import qualified Data.List             as L (drop, foldl, head, last, length, repeat, reverse, take)
import           Data.Maybe            (fromJust, isJust, isNothing)
import           Data.String           ()
import           Interpreter           (Q (..), V (..), ValueP' (..), lengthElem, pruneQ,
                                        pruneV)
import qualified Prelude               as P (replicate, (++))

import           Debug.Trace

newtype Parser a = Parser (V -> Maybe (a, V))

parse :: Parser t -> V -> Maybe (t, V)
parse (Parser p) = p

instance Functor Parser where
 fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
   return a = Parser (\s -> Just (a,s))
   p >>= f  = Parser
    (\s -> do
      (v, s') <- parse p s
      parse (f v) s'
    )

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus Parser where
  mzero = Parser (const Nothing)
  mplus p q = Parser
    (\s -> case parse p s of
      Nothing -> parse q s
      Just x  -> Just x
    )

failure :: Parser a
failure = mzero

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = p `mplus` q

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = item >>= \c -> if p c then return c else failure

char :: Char -> Parser Char
char c = satisfies (c==)

zero :: Integer
zero = 0

one :: Integer
one = 1

item :: Parser Char
item = Parser
  ( \vs -> do
    let vs' = pruneV vs
        p x = x !! zero == '~'
        a x = x <<+ '~'
    case vs' of
      V1024 v -> if p v then Nothing else Just (v !! zero, V1024 (a v))
      V512  v -> if p v then Nothing else Just (v !! zero, V512 (a v))
      V256  v -> if p v then Nothing else Just (v !! zero, V256 (a v))
      V128  v -> if p v then Nothing else Just (v !! zero, V128 (a v))
      V64   v -> if p v then Nothing else Just (v !! zero, V64 (a v))
      V32   v -> if p v then Nothing else Just (v !! zero, V32 (a v))
      V16   v -> if p v then Nothing else Just (v !! zero, V16 (a v))
      V8    v -> if p v then Nothing else Just (v !! zero, V8 (a v))
      V4    v -> if p v then Nothing else Just (v !! zero, V4 (a v))
      V2    v -> if p v then Nothing else Just (v !! zero, V2 (a v))
      _       -> Nothing
  )

-- | Parse a fixed length string.
-- | "abc" == <'a','b','c','~','~','~','~','~','~','~','~','~','~','~','~','~'>
string :: Vec 16 Char -> Parser (Vec 16 Char)
string s = Parser
  ( \vs -> do
    let vs' = pruneV vs
        p :: Vec (16 + n) Char -> Bool
        p x = not (isStrMatch s (take d16 x))
        f :: KnownNat n => Vec n Char -> Vec n Char
        f = popN (lengthElem '~' s) '~'
    case vs' of
      V1024 v -> if p v then Nothing else Just (s, pruneV $ V1024 (f v))
      V512  v -> if p v then Nothing else Just (s, pruneV $ V512 (f v))
      V256  v -> if p v then Nothing else Just (s, pruneV $ V256 (f v))
      V128  v -> if p v then Nothing else Just (s, pruneV $ V128 (f v))
      V64   v -> if p v then Nothing else Just (s, pruneV $ V64 (f v))
      V32   v -> if p v then Nothing else Just (s, pruneV $ V32 (f v))
      V16   v -> if p v then Nothing else Just (s, pruneV $ V16 (f v))
      _       -> Nothing
  )

-- | Same as manyTillChar end character == end of string
manyChar :: Parser Char -> Parser V
manyChar p = manyTillChar p (char '~')

-- manyChar :: Parser Char -> Parser V
-- manyChar p = Parser
--   ( \vs -> do
--     let vs' = pruneV vs
--         mapParser :: KnownNat n => Vec n Char -> Vec n Char
--         mapParser = map (parseChar p)

--         fCnt :: KnownNat n => Vec n Char -> Int
--         fCnt = lengthElem '~'

--         res :: KnownNat n => Vec n Char -> Int -> Vec n Char
--         res v cnt = imap (\i x -> if fromIntegral i < cnt then x else '~') v

--         left :: KnownNat n => Vec n Char -> Vec n Char
--         left v = res (mapParser v) (fCnt (mapParser v))

--         right :: KnownNat n => Vec n Char -> Vec n Char
--         right v = popN (fCnt (mapParser v)) '~' v

--     case vs' of
--       V1024 v -> Just (pruneV $ V1024 (left v), pruneV $ V1024 (right v))
--       V512  v -> Just (pruneV $ V512 (left v), pruneV $ V512 (right v))
--       V256  v -> Just (pruneV $ V256 (left v), pruneV $ V256 (right v))
--       V128  v -> Just (pruneV $ V128 (left v), pruneV $ V128 (right v))
--       V64   v -> Just (pruneV $ V64 (left v), pruneV $ V64 (right v))
--       V32   v -> Just (pruneV $ V32 (left v), pruneV $ V32 (right v))
--       V16   v -> Just (pruneV $ V16 (left v), pruneV $ V16 (right v))
--       V8    v -> Just (pruneV $ V8 (left v), pruneV $ V8 (right v))
--       V4    v -> Just (pruneV $ V4 (left v), pruneV $ V4 (right v))
--       V2    v -> Just (pruneV $ V2 (left v), pruneV $ V2 (right v))
--       _       -> Nothing
--   )

many1Char :: Parser Char -> Parser V
many1Char p = do
  a <- lookAhead p
  if not a then failure else manyChar p
--
manyTillChar :: Parser Char -> Parser Char -> Parser V
manyTillChar p end = Parser
  ( \vs -> do
    let
      vs' = pruneV vs
      mapParser :: KnownNat n => Vec n Char -> Vec n Char
      mapParser v = map (\x -> if x == parseChar end x then '~' else x)
                        (map (parseChar p) v)

      fCnt :: KnownNat n => Vec n Char -> Int
      fCnt = lengthElem '~'

      res :: KnownNat n => Vec n Char -> Int -> Vec n Char
      res v cnt = imap (\i x -> if fromIntegral i < cnt then x else '~') v

      left :: KnownNat n => Vec n Char -> Vec n Char
      left v = res (mapParser v) (fCnt (mapParser v))

      right :: KnownNat n => Vec n Char -> Vec n Char
      right v = popN (fCnt (mapParser v)) '~' v

    case vs' of
      V1024 v -> Just (pruneV $ V1024 (left v), pruneV $ V1024 (right v))
      V512  v -> Just (pruneV $ V512 (left v), pruneV $ V512 (right v))
      V256  v -> Just (pruneV $ V256 (left v), pruneV $ V256 (right v))
      V128  v -> Just (pruneV $ V128 (left v), pruneV $ V128 (right v))
      V64   v -> Just (pruneV $ V64 (left v), pruneV $ V64 (right v))
      V32   v -> Just (pruneV $ V32 (left v), pruneV $ V32 (right v))
      V16   v -> Just (pruneV $ V16 (left v), pruneV $ V16 (right v))
      V8    v -> Just (pruneV $ V8 (left v), pruneV $ V8 (right v))
      V4    v -> Just (pruneV $ V4 (left v), pruneV $ V4 (right v))
      V2    v -> Just (pruneV $ V2 (left v), pruneV $ V2 (right v))
      _       -> Nothing
  )

oneOf :: Vec 16 Char -> Parser Char
oneOf cs = satisfies (`elem`cs)

noneOf :: Vec 16 Char -> Parser Char
noneOf cs = satisfies (`notElem`cs)

lookAhead :: Parser a -> Parser Bool
lookAhead p = Parser
  ( \s -> case parse p s of
    Nothing -> Just (False, s)
    _       -> Just (True, s)
  )

-- | Lexical combinators
-- |
spaces :: Parser ()
spaces = void (manyChar (satisfies isSpace))
 where
  isSpace ' '  = True
  isSpace '\n' = True
  isSpace '\r' = True
  isSpace '\t' = True
  isSpace _    = False

digit :: Parser Char
digit = satisfies isDigit
 where
  isDigit c = isJust (findIndex (==c) digits)
  digits =
    '0' :> '1' :> '2' :> '3' :> '4' :> '5' :> '6' :> '7' :> '8' :> '9' :> Nil

numberInt :: Parser Int
numberInt = do
  sign   <- char '-' <|> digit
  digits <- manyChar digit
  case digits of
    V32 v -> return (fromDigits (sign :> v))
    V16 v -> return (fromDigits (sign :> v))
    V8  v -> return (fromDigits (sign :> v))
    V4  v -> return (fromDigits (sign :> v))
    V2  v -> return (fromDigits (sign :> v))
    _     -> failure

-- -- -- -- numberDouble :: Parser Double
-- -- -- -- numberDouble = do
-- -- -- --   sign     <- string "-" <|> string ""
-- -- -- --   digits   <- many1 digit
-- -- -- --   _        <- string "." <|> string ""
-- -- -- --   mantissa <- many digit
-- -- -- --   _        <- spaces
-- -- -- --   let mantissa' = if mantissa == "" then "0" else mantissa
-- -- -- --       double    = sign ++ digits ++ "." ++ mantissa'
-- -- -- --   return (read double :: Double)


letter :: Parser Char
letter = satisfies isAlpha where isAlpha c = isJust (findIndex (==c) letters)

azLower :: Vec 26 Char
azLower =
  'a'
    :> 'b'
    :> 'c'
    :> 'd'
    :> 'e'
    :> 'f'
    :> 'g'
    :> 'h'
    :> 'i'
    :> 'j'
    :> 'k'
    :> 'l'
    :> 'm'
    :> 'n'
    :> 'o'
    :> 'p'
    :> 'q'
    :> 'r'
    :> 's'
    :> 't'
    :> 'u'
    :> 'v'
    :> 'w'
    :> 'x'
    :> 'y'
    :> 'z'
    :> Nil

azUpper :: Vec 26 Char
azUpper =
  'A'
    :> 'B'
    :> 'C'
    :> 'D'
    :> 'E'
    :> 'F'
    :> 'G'
    :> 'H'
    :> 'I'
    :> 'J'
    :> 'K'
    :> 'L'
    :> 'M'
    :> 'N'
    :> 'O'
    :> 'P'
    :> 'Q'
    :> 'R'
    :> 'S'
    :> 'T'
    :> 'U'
    :> 'V'
    :> 'W'
    :> 'X'
    :> 'Y'
    :> 'Z'
    :> Nil

letters :: Vec 52 Char
letters = azLower ++ azUpper

firstLetter :: Parser Char
firstLetter = letter <|> oneOf symbols
 where
  symbols =
    '+'
      :> '-'
      :> '*'
      :> '/'
      :> '<'
      :> '>'
      :> '='
      :> '!'
      :> '?'
      :> '$'
      :> '%'
      :> '&'
      :> '@'
      :> '´'
      :> '\''
      :> '_'
      :> Nil

wordLetter :: Parser Char
wordLetter = firstLetter <|> digit

newline :: Parser Char
newline = char '\n'

crlf :: Parser Char
crlf = char '\r' *> char '\n'

endOfLine :: Parser Char
endOfLine = newline <|> crlf

anyChar :: Parser Char
anyChar = satisfies (const True)

escapeNewLine :: Parser Char
escapeNewLine = do
  b <- lookAhead (string ('\\' +>> '\n' +>> blank16))
  if b
    then do
      _ <- char '\\'
      char '\n'
    else failure

nonEscape :: Parser Char
nonEscape = noneOf ('\\' +>> '\"' +>> blank16)

quotedString :: Parser V
quotedString = do
  char '"'
  s <- manyChar (escapeNewLine <|> nonEscape)
  char '"'
  return $ pruneV s

-- | Pointless specific parsers
--
numberP :: Parser ValueP'
numberP = do
  d <- numberInt
  return (NumP' d)

charP :: Parser ValueP'
charP = do
  _ <- char '\''
  c <- newline <|> firstLetter
  _ <- char '\''
  return (Chr' c)

quotedStringP :: Parser ValueP'
quotedStringP = do
  str <- quotedString
  case str of
    V2    v -> return (Str' (V2 v))
    V4    v -> return (Str' (V4 v))
    V8    v -> return (Str' (V8 v))
    V16   v -> return (Str' (V16 v))
    V32   v -> return (Str' (V32 v))
    V64   v -> return (Str' (V64 v))
    V128  v -> return (Str' (V128 v))
    V256  v -> return (Str' (V256 v))
    V512  v -> return (Str' (V512 v))
    V1024 v -> return (Str' (V1024 v))
    _       -> failure

word :: Parser ValueP'
word = do
  c  <- firstLetter
  cs <- manyChar wordLetter
  case cs of
    V2  v -> return (Sym' (c :> select d0 d1 d2 v ++ drop d3 blank16))
    V4  v -> return (Sym' (c :> select d0 d1 d4 v ++ drop d5 blank16))
    V8  v -> return (Sym' (c :> select d0 d1 d8 v ++ drop d9 blank16))
    V16 v -> return (Sym' (select d0 d1 d16 (c +>> v)))
    _     -> failure

lineComment :: Parser ()
lineComment = char '$' >> manyTillChar anyChar newline >> spaces >> return ()

blockComment :: Parser ()
blockComment =
  char '{' >> manyTillChar anyChar (char '}') >> char '}' >> spaces >> return ()

comment :: Parser ()
comment = lineComment <|> blockComment

manyEmpty :: Parser () -> Parser ()
manyEmpty p = do
  com <- lookAhead p
  if com
    then do
      _ <- p
      _ <- manyEmpty p
      return ()
    else return ()

comments :: Parser ()
comments = manyEmpty comment

specification :: Parser ()
specification =
  char '(' >> manyTillChar anyChar (char ')') >> char ')' >> spaces >> return ()

specifications :: Parser ()
specifications = manyEmpty specification

spacesCommentsSpecifications :: Parser ()
spacesCommentsSpecifications = spaces >> comments >> specifications >> comments

-- |parsers to get inline test from inside {}
lineComments :: Parser ()
lineComments = manyEmpty lineComment

spacesLineCommentsSpecifications :: Parser ()
spacesLineCommentsSpecifications =
  spaces >> lineComments >> specifications >> lineComments
--
parseValueP :: Parser ValueP' -> V -> ValueP'
parseValueP p vs = case parse p vs of
  Just (p, _) -> p
  Nothing     -> EmptyQ

manyQ :: Parser ValueP' -> Parser Q
manyQ p = Parser
  ( \vs -> case parseValueP p vs of
    EmptyQ -> Nothing
    _      -> Just (mP p vs)
  )

mP :: Parser ValueP' -> V -> (Q, V)
mP p_ vs_ = (pruneQ (Q1024 resQ'), resS)
 where
  (resQ, resS) = go p_ vs_ (repeat EmptyQ :: Vec 1024 ValueP')
  cnt          = foldl (\acc v -> if v == EmptyQ then acc + 1 else acc) 0 resQ
  resQ'        = rotateLeft resQ cnt
  go p vs qs = case parseValueP p vs of
    EmptyQ -> (qs, vs)
    _      -> do
      let (vp, vs') = fromJust $ parse p vs
      go p vs' (qs <<+ vp)
--
emptyQuot :: Parser ValueP'
emptyQuot = do
  l <- char '['
  r <- char ']'
  return (Quot' (Q2 (Chr' l :> Chr' r :> Nil)))

instruction :: Parser ValueP'
instruction = do
  _   <- spacesCommentsSpecifications
  -- res <- numberP <|> charP <|> quotedStringP <|> emptyQuot <|> word 
  res <-
    numberP <|> charP <|> quotedStringP <|> emptyQuot <|> quotation <|> word
  _ <- spacesCommentsSpecifications
  return res

nakedQuotations :: Parser Q
nakedQuotations = manyQ instruction

quotation :: Parser ValueP'
quotation = do
  _ <- char '['
  _ <- spaces
  q <- nakedQuotations
  -- traceM $ "\nq: " ++ show q
  _ <- spaces
  _ <- char ']'
  return (Quot' q)

-- -- nonTest :: Parser ()
-- -- nonTest = do
-- --   _ <- spacesLineCommentsSpecifications
-- --   _ <- numberP <|> charP <|> quotedStringP <|> quotation <|> word
-- --   _ <- spacesLineCommentsSpecifications
-- --   return ()

-- -- nonTests :: Parser [()]
-- -- nonTests = many nonTest

-- -- testBlock :: Parser String
-- -- testBlock = do
-- --   _ <- char '{'
-- --   s <- manyTill anyChar (char '}')
-- --   _ <- char '}'
-- --   _ <- spaces
-- --   return s

-- -- test :: Parser String
-- -- test = do
-- --   _ <- many nonTest
-- --   t <- testBlock
-- --   _ <- many nonTest
-- --   return t

-- -- tests :: Parser [String]
-- -- tests = many test




-- -- | Helper functions.
-- -- |
-- -- strCharCount :: Vec n Char -> Int
-- -- strCharCount = foldl (\acc c -> if c /= '~' then acc + 1 else acc) 0

isStrMatch :: Vec 16 Char -> Vec 16 Char -> Bool
isStrMatch xs vs = foldl (&&) True zipped
  where zipped = zipWith (\x v -> x == v || x == '~') xs vs

-- | get the character by applying parser p or '~' if Nothing
parseChar :: Parser Char -> Char -> Char
parseChar p c = case parse p vs of
  Just (x, _) -> x
  Nothing     -> '~'
  where vs = V2 (c :> c :> Nil)

-- | popN n chars from vs
popN :: KnownNat n => Int -> a -> Vec n a -> Vec n a
popN 0   _ vs = vs
popN cnt c vs = popN (cnt - 1) c (vs <<+ c)

-- -- | Count non '~' consecutive charaters starting a Vector
-- lengthElem :: (Eq a, KnownNat n) => a -> Vec n a -> Int
-- lengthElem a vs = case findIndex (==a) vs of
--   Just n -> fromIntegral (toInteger n)
--   _      -> length vs

-- | Covert a vector of chars to an int
-- | takes chars != '~'
fromDigits :: KnownNat n => Vec n Char -> Int
fromDigits vs = val * sign
 where
  isMinus = vs !! (0 :: Integer) == '-'
  vs'     = if isMinus then vs <<+ '~' else vs
  sign    = if isMinus then (-1) else 1
  val =
    foldl (\acc c -> if c /= '~' then 10 * acc + C.digitToInt c else acc) 0 vs'


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

blank16 :: Vec 16 Char
blank16 = repeat '~'

blank64 :: Vec 64 Char
blank64 = repeat '~'

blank1024 :: Vec 1024 Char
blank1024 = repeat '~'

blank65536 :: Vec 65536 Char
blank65536 = repeat '~'

-- xs' = parse nakedQuotations  (loadStr "[10 20 +] dup exec swap exec + [10 20 +] dup exec swap exec +")
-- ss' = showParse xs'

-- -- | Parser tests.
-- -- |

p001Src :: V
p001Src =
  loadStr
    "\"p001\" [ 1 n from-to-list [ [3 is-div-by] [5 is-div-by] cleave or ] filter sum ] define"

aaa :: IO ()
aaa = do
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
      r23 = "(NumP' 123, \" abc\")"
  putStr $ "parse numberP:           " P.++ show (r23 == showParse s23)
  putStrLn $ ",  result = " P.++ showParse s23

  let s24 = parse numberP (loadStr "-123 abc")
      r24 = "(NumP' (-123), \" abc\")"
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



