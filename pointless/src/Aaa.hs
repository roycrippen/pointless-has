{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Clash.Prelude hiding (many, (<|>))
import           Control.Monad (ap, liftM, void)
import qualified Data.Char     as C (digitToInt)
import qualified Data.List     as L (find, foldl, length, repeat, reverse, take)
import           Data.Maybe    (fromJust, isJust)
import           Data.String   ()
import qualified Prelude       as P ((++))

newtype Parser a = Parser (Vec 32 Char -> Maybe (a, Vec 32 Char))

parse :: Parser t -> Vec 32 Char -> Maybe (t, Vec 32 Char)
parse (Parser p) = p

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
   return a = Parser (\s -> Just (a,s))
   p >>= f  = Parser (\s -> do (v, s') <- parse p s
                               parse (f v) s')

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus Parser where
  mzero = Parser (\_ -> Nothing)
  mplus p q = Parser (\s -> case parse p s of
                              Nothing -> parse q s
                              Just x  -> Just x)

failure :: Parser a
failure = mzero

item :: Parser Char
item = Parser item'
 where
  item' vs = if  vs !! zero == '~'
    then Nothing
    else Just (vs !! zero, (replaceThenRotate '~' vs ))

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = p `mplus` q

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = item >>= \c -> if p c then return c else failure

char :: Char -> Parser Char
char c = satisfies (c ==)

isStrMatch :: Vec 16 Char -> Vec 32 Char -> Bool
isStrMatch xs vs = foldl (&&) (True) zipped
  where zipped = zipWith (\x v  -> x == v || x == '~')  xs (takeI vs :: Vec 16 Char)

-- | Parse a fixed length string.
-- | "abc" == <'a','b','c','~','~','~','~','~','~','~','~','~','~','~','~','~'>
string :: Vec 16 Char -> Parser (Vec 16 Char)
string vs = Parser
    ( \s -> if isStrMatch vs s
      then Just (vs, replaceThenRotateN (strCharCount vs) '~' s)
      else Nothing
    )

manyChar :: Parser Char -> Parser (Vec 32 Char)
manyChar p = do
  b <- lookAhead p
  if not b
    then Parser (\s -> Just(str32 '~', s))
    else getManyChar p

many1Char :: Parser Char -> Parser (Vec 32 Char)
many1Char p = do
  b <- lookAhead p
  if not b
    then failure
    else getManyChar p

getManyChar :: Parser Char -> Parser (Vec 32 Char)
getManyChar p = Parser
  ( \vs -> do
    let vs' = map (\x -> parseChar p (str32 x)) vs
        cnt = cntConsecutive vs'
        res = imap (\i x ->  if (fromIntegral i < cnt) then x else '~') vs'
    Just (res, replaceThenRotateN cnt '~' vs)
  )

parseChar :: Parser Char -> Vec 32 Char -> Char
parseChar p vs = do
  case parse p vs of
    Just (c, _) -> c
    Nothing     -> '~'

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
 where
  rest a =
    ( do
        f <- op
        b <- p
        rest (f a b)
      )
      <|> return a

oneOf :: Vec 16 Char -> Parser Char
oneOf cs = satisfies (`elem` cs)

noneOf :: Vec 16 Char -> Parser Char
noneOf cs = satisfies (`notElem` cs)

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = manyTill1 p end <|> return []

manyTill1 :: Parser a -> Parser b -> Parser [a]
manyTill1 p end = do
  a <- p
  b <- lookAhead end
  if b
    then return [a]
    else do
      as <- manyTill p end
      return (a : as)

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
    isDigit c = isJust (findIndex (== c) digits)
    digits = '0':>'1':>'2':>'3':>'4':>'5':>'6':>'7':>'8':>'9':>Nil

numberInt :: Parser Int
numberInt = do
  sign   <- char '-' <|> digit
  digits <- many1Char digit
  return (fromDigits (sign +>> digits))

-- -- -- numberDouble :: Parser Double
-- -- -- numberDouble = do
-- -- --   sign     <- string "-" <|> string ""
-- -- --   digits   <- many1 digit
-- -- --   _        <- string "." <|> string ""
-- -- --   mantissa <- many digit
-- -- --   _        <- spaces
-- -- --   let mantissa' = if mantissa == "" then "0" else mantissa
-- -- --       double    = sign ++ digits ++ "." ++ mantissa'
-- -- --   return (read double :: Double)

letter :: Parser Char
letter = satisfies isAlpha
 where
  isAlpha c = isJust (findIndex (== c) letters)
  letters = 'a':>'b':>'c':>'d':>'e':>'f':>'g':>'h':>'i':>'j':>'k':>'l':>'m':>'n':>'o':>'p':>'q':>Nil ++
            'r':>'s':>'t':>'u':>'v':>'w':>'x':>'y':>'z':>'A':>'B':>'C':>'D':>'E':>'F':>'G':>'H':>Nil ++
            'I':>'J':>'K':>'L':>'M':>'N':>'O':>'P':>'Q':>'R':>'S':>'T':>'U':>'V':>'W':>'X':>'Y':>'Z':>Nil

firstLetter :: Parser Char
firstLetter = letter <|> oneOf symbols
  where symbols = '+':>'-':>'*':>'/':>'<':>'>':>'=':>'!':>'?':>'$':>'%':>'&':>'@':>'´':>'\'':>'_':>Nil

wordLetter :: Parser Char
wordLetter = firstLetter <|> digit

newline :: Parser Char
newline = char '\n'

crlf :: Parser Char
crlf = char '\r' *> char '\n'

-- endOfLine :: Parser Char
-- endOfLine = newline <|> crlf

-- anyChar :: Parser Char
-- anyChar = satisfies (const True)

-- emptyQuot :: Parser String
-- emptyQuot = string "[]"

-- escapeNewLine :: Parser Char
-- escapeNewLine = do
--   b <- lookAhead (string "\\\n")
--   -- traceM $ "\nb: " ++ show b
--   if b
--     then do
--       _ <- char '\\'
--       char '\n'
--     else mzero

-- nonEscape :: Parser Char
-- nonEscape = noneOf "\\\""

-- -- character :: Parser Char
-- -- character = nonEscape <|> escapeNewLine

-- quotedString :: Parser String
-- quotedString = do
--   char '"'
--   strings <- many (escapeNewLine <|> nonEscape)
--   char '"'
--   return strings


-- | Helper functions.
-- |
str8 :: Char -> Vec 8 Char
str8 = repeat

str16 :: Char -> Vec 16 Char
str16 = repeat

str32 :: Char -> Vec 32 Char
str32 = repeat

str64 :: Char -> Vec 64 Char
str64 = repeat

str1024 :: Char -> Vec 1024 Char
str1024 = repeat

strCharCount :: Vec n Char -> Int
strCharCount xs = foldl (\acc c -> if c /= '~' then acc + 1 else acc) 0 xs

manyCharCnt :: Char -> Vec n Char -> Int
manyCharCnt ch vs = (foldl (\acc c -> if c == ch then acc + 1 else acc) 0 vs') - 1
  where vs' = scanl (\acc c -> if acc == c then acc else '~') ch vs

zero :: Integer
zero = 0

one :: Integer
one = 1

replaceThenRotate :: forall (n :: Nat). KnownNat n => Char -> Vec n Char -> Vec n Char
replaceThenRotate c vs =  rotateLeft (replace zero c vs) one

replaceThenRotateN :: forall (n :: Nat). KnownNat n => Int -> Char -> Vec n Char -> Vec n Char
replaceThenRotateN 0 _ vs =  vs
replaceThenRotateN cnt c vs =
  replaceThenRotateN (cnt - 1) c (replaceThenRotate c vs)

-- | Count non '~' consecutive charaters starting a Vector
cntConsecutive :: KnownNat n => Vec n Char -> Int
cntConsecutive vs = go 0 vs vs
    where
      go :: KnownNat n1 => Int -> Vec n1 Char -> Vec n1 Char -> Int
      go cnt orig new  = do
        let new' = rotateLeft new one
        if (new !! 0) == '~' || new' == orig
          then cnt
          else do
            go (cnt + 1) orig new'

-- | Covert a vector of chars to an int
-- | takes chars != '~'
fromDigits :: KnownNat n => Vec n Char -> Int
fromDigits vs = val * sign
  where
    isMinus = vs !! (0::Integer) == '-'
    vs'  = if isMinus then replaceThenRotate '~' vs else vs
    sign = if isMinus then (-1) else 1
    val  = foldl  ( \acc c -> if c /= '~'
                              then 10 * acc + (C.digitToInt c)
                              else acc
                  ) 0 vs'

loadStr8 :: String -> Vec 8 Char
loadStr8 s = go s' (str8 '~')
  where
    s' = strToCharN 8 s
    go "" vs     = vs
    go (c:cs) vs = go cs (replaceThenRotate c vs)

loadStr16 :: String -> Vec 16 Char
loadStr16 s = go s' (str16 '~')
  where
    s' = strToCharN 16 s
    go "" vs     = vs
    go (c:cs) vs = go cs (replaceThenRotate c vs)

loadStr32 :: String -> Vec 32 Char
loadStr32 s = go s' (str32 '~')
  where
    s' = strToCharN 32 s
    go "" vs     = vs
    go (c:cs) vs = go cs (replaceThenRotate c vs)

strToCharN :: Int -> String -> [Char]
strToCharN n s
  | n > len   = s P.++ (L.take (n - len) (L.repeat '~'))
  | n == len  = s
  | otherwise = L.take n s
    where len = L.length s

vecToString :: Vec n Char -> String
vecToString vs = show $ L.reverse $ foldl (\acc c -> if c /= '~' then c : acc else acc) "" vs

showParseResult :: Show a => Maybe (a, Vec 32 Char) -> String
showParseResult res = if isJust res
  then do
    let (r, vec) = fromJust res
    "(" P.++ show r P.++ ", " P.++ (vecToString vec) P.++ ")"
  else "Nothing"

-- | Data for tests
-- |
vs_ :: Vec 32 Char
vs_ = loadStr32 "abc   123"

abc :: Vec 16 Char
abc = loadStr16 "abc"

aaa :: Vec 32 Char
aaa = loadStr32 "aaa bbb aaa"

s16 :: Vec 16 Char
s16 = loadStr16 ['a' .. 'p']

main :: IO ()
main = do
  putStrLn "Pointless in Clash tests\n"
  parserTests

parserTests :: IO ()
parserTests = do
  putStrLn "parserTests..."

  let vs = vs_
  putStrLn $ "Vec: " P.++ (show vs)
  putStrLn $ "back to str: " P.++ (vecToString vs) P.++ "\n"

  let s0 = showParseResult $ parse (oneOf $ loadStr16 "cdef") vs_
  putStrLn $  "parse (oneOf $ loadStr16 \"cdef\"): " P.++ s0

  let s1 = showParseResult $ parse (oneOf $ loadStr16 "aaa bbb aaa") vs_
  putStrLn $  "parse (oneOf loadStr32 \"aaa bbb aaa\"): " P.++ s1

  let s2 = showParseResult $ parse (string $ loadStr16 "abc") vs_
  putStrLn $  "parse (string loadStr16 \"abc\"): " P.++ s2

  let s3 = showParseResult $ parse ((string $ loadStr16 "abc") >> spaces) vs_
  putStrLn $  "parse ((string loadStr16 \"abc\") >> spaces): " P.++ s3

  let s4 = showParseResult $ parse ((string $ loadStr16 "abc") >> spaces >> digit) vs_
  putStrLn $  "parse ((string loadStr16 \"abc\") >> spaces >> digit): " P.++ s4

  let s5 = showParseResult $ parse ((string $ loadStr16 "abc") >> spaces >> numberInt) vs_
  putStrLn $  "parse ((string loadStr16 \"abc\") >> spaces >> numberInt): " P.++ s5

  let s6 = showParseResult $ parse numberInt (loadStr32 "123 abc")
  putStrLn $  "parse numberInt (loadStr32 \"123 abc\"): " P.++ s6

  let s7 = show $ manyCharCnt 'a' (loadStr16 "aaa bbb")
  putStrLn $  "manyCharCnt 'a' (loadStr16 \"aaa bbb\"): " P.++ s7

  let s8 = show $ manyCharCnt 'a' (loadStr16 "a bbb")
  putStrLn $  "manyCharCnt 'a' (loadStr16 \"a bbb\"): " P.++ s8

  let s9 = show $ manyCharCnt 'a' (loadStr16 "bbb aaa")
  putStrLn $  "manyCharCnt 'a' (loadStr16 \"bbb aaa\"): " P.++ s9

  let s10 = showParseResult $ parse (manyChar (char 'a')) (loadStr32 "aaa bbb")
  putStrLn $  "manyChar 'a' (loadStr32 \"aaa bbb\"): " P.++ s10

  let s11 = showParseResult $ parse (manyChar (char 'a')) (loadStr32 "a bbb")
  putStrLn $  "manyChar 'a' (loadStr32 \"a bbb\"): " P.++ s11

  let s12 = showParseResult $ parse (manyChar (char 'a')) (loadStr32 "bbb aaa")
  putStrLn $  "manyChar 'a' (loadStr32 \"bbb aaa\"): " P.++ s12

  let s13 = showParseResult $ parse (many1Char (char 'a')) (loadStr32 "aaa bbb")
  putStrLn $  "many1Char 'a' (loadStr32 \"aaa bbb\"): " P.++ s13

  let s14 = showParseResult $ parse (many1Char (char 'a')) (loadStr32 "a bbb")
  putStrLn $  "many1Char 'a' (loadStr32 \"a bbb\"): " P.++ s14

  let s15 = showParseResult $ parse (many1Char (char 'a')) (loadStr32 "bbb aaa")
  putStrLn $  "many1Char 'a' (loadStr32 \"bbb aaa\"): " P.++ s15


--   let s1 = parse quotedString "\"hello world\" 123"
--   putStrLn $ "parse quotedString: " P.++ show s1 P.++ "\n"

--   let c1 = parse firstLetter "abc"
--   putStrLn $ "parse firstLetter: " P.++ show c1 P.++ "\n"

--   let c2 = parse firstLetter "_abc"
--   putStrLn $ "parse firstLetter: " P.++ show c2 P.++ "\n"

  -- let (c3, _):_ =  parse charP "'z'"
  -- putStrLn $ "parse charP: " ++ if (c3 == (Chr 'z')) then "OK" else "ERROR"
  -- putStrLn $ show c3 ++ "\n"

  -- let (v0, _):_ = parse quotedStringP "\"abc\""
  --     v0Str =  if (v0 == Str "abc") then "OK" else "ERROR"
  -- putStrLn $ "parse quotedString: " ++ v0Str
  -- putStrLn $ show v0 ++ "\n"

  -- let (v1, _):_ = parse numberP "-23"
  --     v1Str =  if (v1 == (NumP (-23))) then "OK" else "ERROR"
  -- putStrLn $ "parse numberP: " ++ v1Str
  -- putStrLn $ show v1 ++ "\n"

  -- let (v2, _):_ = parse nakedQuotations "-10 10 +"
  --     v2Str =  if (v2 == [NumP (-10),NumP 10,Sym "+"]) then "OK" else "ERROR"
  -- putStrLn $ "parse nakedQuotation: " ++ v2Str
  -- putStrLn $ show v2 ++ "\n"

  -- let (v3, _):_ = parse nakedQuotations " 10 \"aaa\" set-var 3 dup "
  --     v3Str =  if (v3 == [NumP 10,Str "aaa",Sym "set-var", NumP 3, Sym "dup"]) then "OK" else "ERROR"
  -- putStrLn $ "parse nakedQuotation: " ++ v3Str
  -- putStrLn $ show v3 ++ "\n"


  -- strV :: forall (n :: Nat). KnownNat n => Nat -> Char -> Vec n Char
-- strV :: Nat -> Char -> Vec n Char
-- strV cnt = replicate (SNat :: SNat cnt)

