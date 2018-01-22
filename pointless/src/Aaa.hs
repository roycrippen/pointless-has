{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import Clash.Prelude hiding ((<|>))
import           Control.Monad (ap, liftM, void)
import qualified Data.Char     as C (digitToInt)
import qualified Data.List     as L (find, foldl, length, repeat, reverse, take)
import           Data.Maybe    (fromJust, isJust)
import           Data.String   ()
import qualified Prelude       as P (replicate, (++))

import Interpreter (ValueP'(..))

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

item :: Parser Char
item = Parser item'
 where
  item' vs = if vs !! zero == '~'
    then Nothing
    else Just (vs !! zero, replaceThenRotate '~' vs)

-- | Parse a fixed length string.
-- | "abc" == <'a','b','c','~','~','~','~','~','~','~','~','~','~','~','~','~'>
string :: Vec 16 Char -> Parser (Vec 16 Char)
string vs = Parser
  ( \s -> if isStrMatch vs s
    then Just (vs, replaceThenRotateN (strCharCount vs) '~' s)
    else Nothing
  )

manyChar :: Parser Char -> Parser (Vec 32 Char)
manyChar p = Parser
  ( \vs -> do
    let vs' = map (parseChar p . str32) vs
        cnt = cntConsecutive vs'
        res = imap (\i x -> if fromIntegral i < cnt then x else '~') vs'
    Just (res, replaceThenRotateN cnt '~' vs)
  )

many1Char :: Parser Char -> Parser (Vec 32 Char)
many1Char p = do
  a <- lookAhead p
  if not a then failure else manyChar p

manyTillChar :: Parser Char -> Parser Char -> Parser (Vec 32 Char)
manyTillChar p end = Parser
  ( \vs -> do
    let vs'  = map (parseChar p . str32) vs
        vs'' = map (\x -> if x == parseChar end (str32 x) then '~' else x) vs'
        cnt  = cntConsecutive vs''
        res  = imap (\i x -> if fromIntegral i < cnt then x else '~') vs'
    Just (res, replaceThenRotateN cnt '~' vs)
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

emptyQuot :: Parser (Vec 16 Char)
emptyQuot = string ('[' +>> ']' +>> str16 '~')

escapeNewLine :: Parser Char
escapeNewLine = do
  b <- lookAhead (string ('\\' +>> '\n' +>> str16 '~'))
  if b
    then do
      _ <- char '\\'
      char '\n'
    else failure

nonEscape :: Parser Char
nonEscape = noneOf ('\\' +>> '\"' +>> str16 '~')

quotedString :: Parser (Vec 32 Char)
quotedString = do
  char '"'
  s <- manyChar (escapeNewLine <|> nonEscape)
  char '"'
  return s

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
  return (Str' str)

word :: Parser ValueP'
word = do
  c  <- firstLetter
  cs <- manyChar wordLetter
  return (Sym' (select d0 d1 d16 (c +>> cs)))

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

-- instruction :: Parser ValueP'
-- instruction = do
--   _   <- spacesCommentsSpecifications
--   res <- numberP <|> charP <|> quotedStringP <|> quotation <|> word
--   _   <- spacesCommentsSpecifications
--   return res

-- nakedQuotations :: Parser [ValueP']
-- nakedQuotations = many instruction

-- quotation :: Parser ValueP'
-- quotation = do
--   _ <- char '['
--   _ <- spaces
--   q <- nakedQuotations
--   -- traceM $ "\nq: " ++ show q
--   _ <- spaces
--   _ <- char ']'
--   return (Quot' q)

-- nonTest :: Parser ()
-- nonTest = do
--   _ <- spacesLineCommentsSpecifications
--   _ <- numberP <|> charP <|> quotedStringP <|> quotation <|> word
--   _ <- spacesLineCommentsSpecifications
--   return ()

-- nonTests :: Parser [()]
-- nonTests = many nonTest

-- testBlock :: Parser String
-- testBlock = do
--   _ <- char '{'
--   s <- manyTill anyChar (char '}')
--   _ <- char '}'
--   _ <- spaces
--   return s

-- test :: Parser String
-- test = do
--   _ <- many nonTest
--   t <- testBlock
--   _ <- many nonTest
--   return t

-- tests :: Parser [String]
-- tests = many test




-- | Helper functions.
-- |
strCharCount :: Vec n Char -> Int
strCharCount = foldl (\acc c -> if c /= '~' then acc + 1 else acc) 0

isStrMatch :: Vec 16 Char -> Vec 32 Char -> Bool
isStrMatch xs vs = foldl (&&) True zipped
 where
  zipped = zipWith (\x v -> x == v || x == '~') xs (takeI vs :: Vec 16 Char)

parseChar :: Parser Char -> Vec 32 Char -> Char
parseChar p vs = case parse p vs of
  Just (c, _) -> c
  Nothing     -> '~'

zero :: Integer
zero = 0

one :: Integer
one = 1

replaceThenRotate
  :: forall (n :: Nat) . KnownNat n => Char -> Vec n Char -> Vec n Char
replaceThenRotate c vs = rotateLeft (replace zero c vs) one

replaceThenRotateN
  :: forall (n :: Nat) . KnownNat n => Int -> Char -> Vec n Char -> Vec n Char
replaceThenRotateN 0 _ vs = vs
replaceThenRotateN cnt c vs =
  replaceThenRotateN (cnt - 1) c (replaceThenRotate c vs)

-- | Count non '~' consecutive charaters starting a Vector
cntConsecutive :: KnownNat n => Vec n Char -> Int
cntConsecutive vs = go 0 vs vs
 where
  go :: KnownNat n1 => Int -> Vec n1 Char -> Vec n1 Char -> Int
  go cnt orig new = do
    let new' = rotateLeft new one
    if (new !! 0) == '~' || new' == orig then cnt else go (cnt + 1) orig new'

-- | Covert a vector of chars to an int
-- | takes chars != '~'
fromDigits :: KnownNat n => Vec n Char -> Int
fromDigits vs = val * sign
 where
  isMinus = vs !! (0 :: Integer) == '-'
  vs'     = if isMinus then replaceThenRotate '~' vs else vs
  sign    = if isMinus then (-1) else 1
  val =
    foldl (\acc c -> if c /= '~' then 10 * acc + C.digitToInt c else acc) 0 vs'

strToCharN :: Int -> String -> String
strToCharN n s | n > len   = s P.++ P.replicate (n - len) '~'
               | n == len  = s
               | otherwise = L.take n s
  where len = L.length s

vecToString :: Vec n Char -> String
vecToString vs =
  show $ L.reverse $ foldl (\acc c -> if c /= '~' then c : acc else acc) "" vs

showParseResult :: Show a => Maybe (a, Vec 32 Char) -> String
showParseResult res = if isJust res
  then do
    let (r, vec) = fromJust res
    "(" P.++ show r P.++ ", " P.++ vecToString vec P.++ ")"
  else "Nothing"

str16 :: Char -> Vec 16 Char
str16 = repeat

str32 :: Char -> Vec 32 Char
str32 = repeat

str1024 :: Char -> Vec 1024 Char
str1024 = repeat

loadStr16 :: String -> Vec 16 Char
loadStr16 s = go s' (str16 '~')
 where
  s' = strToCharN 16 s
  go ""     vs = vs
  go (c:cs) vs = go cs (replaceThenRotate c vs)

loadStr32 :: String -> Vec 32 Char
loadStr32 s = go s' (str32 '~')
 where
  s' = strToCharN 32 s
  go ""     vs = vs
  go (c:cs) vs = go cs (replaceThenRotate c vs)
--

consVec :: a -> Vec n a -> Vec (n + 1) a
consVec c vs = c :> vs



padStrN :: Int -> String -> String
padStrN n s = s P.++ P.replicate (n - L.length s) '~'

blank1024 :: Vec 1024 Char
blank1024 = repeat '~'

-- | Data for tests
-- |

main :: IO ()
main = do
  putStrLn "Pointless in Clash tests\n"
  parserTests

parserTests :: IO ()
parserTests = do
  putStrLn "parserTests..."

  let vs = loadStr32 "abc 123"
  putStrLn $ "Vec: " P.++ show vs
  putStrLn $ "back to str: " P.++ vecToString vs P.++ "\n"

  let s1 = showParseResult
        $ parse (oneOf $ loadStr16 "cba") (loadStr32 "abc 123")
  putStrLn
    $    "parse (oneOf $ loadStr16 \"cba\") (loadStr32 \"abc 123\"): "
    P.++ s1

  let
    s2 =
      showParseResult $ parse (string $ loadStr16 "abc") (loadStr32 "abc   123")
  putStrLn
    $    "parse (string loadStr16 \"abc\") (loadStr32 \"abc   123\"): "
    P.++ s2

  let s3 = showParseResult
        $ parse (string (loadStr16 "abc") >> spaces) (loadStr32 "abc   123")
  putStrLn
    $ "parse ((string loadStr16 \"abc\") >> spaces) (loadStr32 \"abc   123\"): "
    P.++ s3

  let s4 = showParseResult $ parse
        (string (loadStr16 "abc") >> spaces >> digit)
        (loadStr32 "abc   123")
  putStrLn
    $ "parse ((string loadStr16 \"abc\") >> spaces >> digit) (loadStr32 \"abc   123\"): "
    P.++ s4

  let s5 = showParseResult $ parse
        (string (loadStr16 "abc") >> spaces >> numberInt)
        (loadStr32 "abc   123")
  putStrLn
    $ "parse ((string loadStr16 \"abc\") >> spaces >> numberInt) (loadStr32 \"abc   123\"): "
    P.++ s5

  let s6 = showParseResult $ parse numberInt (loadStr32 "123 abc")
  putStrLn $ "parse numberInt (loadStr32 \"123 abc\"): " P.++ s6

  let
    s7 =
      showParseResult $ parse (oneOf $ loadStr16 "defa") (loadStr32 "abc   123")
  putStrLn
    $    "parse (oneOf $ loadStr16 \"defa\") (loadStr32 \"abc   123\"): "
    P.++ s7

  let
    s8 =
      showParseResult $ parse (oneOf $ loadStr16 "cdef") (loadStr32 "abc   123")
  putStrLn
    $    "parse (oneOf $ loadStr16 \"cdef\") (loadStr32 \"abc   123\"): "
    P.++ s8

  let
    s9 =
      showParseResult $ parse (noneOf $ loadStr16 "def") (loadStr32 "abc   123")
  putStrLn
    $    "parse (noneOf $ loadStr16 \"def\") (loadStr32 \"abc   123\"): "
    P.++ s9

  let s10 = showParseResult $ parse (manyChar (char 'a')) (loadStr32 "aaa bbb")
  putStrLn $ "parse manyChar 'a' (loadStr32 \"aaa bbb\"): " P.++ s10

  let s11 = showParseResult $ parse (manyChar (char 'a')) (loadStr32 "a bbb")
  putStrLn $ "parse manyChar 'a' (loadStr32 \"a bbb\"): " P.++ s11

  let s12 = showParseResult $ parse (manyChar (char 'a')) (loadStr32 "bbb aaa")
  putStrLn $ "parse manyChar 'a' (loadStr32 \"bbb aaa\"): " P.++ s12

  let s13 =
        showParseResult $ parse (many1Char (char 'a')) (loadStr32 "aaa bbb")
  putStrLn $ "parse many1Char 'a' (loadStr32 \"aaa bbb\"): " P.++ s13

  let s14 = showParseResult $ parse (many1Char (char 'a')) (loadStr32 "a bbb")
  putStrLn $ "parse many1Char 'a' (loadStr32 \"a bbb\"): " P.++ s14

  let s15 =
        showParseResult $ parse (many1Char (char 'a')) (loadStr32 "bbb aaa")
  putStrLn $ "parse many1Char 'a' (loadStr32 \"bbb aaa\"): " P.++ s15

  let s16 = showParseResult $ parse emptyQuot (loadStr32 "[] abc")
  putStrLn $ "parse emptyQuot (loadStr32 \"[] abc\"): " P.++ s16

  let s17 = showParseResult
        $ parse (manyTillChar anyChar (char '}')) (loadStr32 "123} ccc")
  putStrLn
    $    "parse (manyTillChar anyChar (char '}'))  (loadStr32 \"123} ccc\"): "
    P.++ s17

  let s18 =
        showParseResult $ parse quotedString (loadStr32 "\"hello world\" 123")
  putStrLn
    $    "parse quotedString (loadStr32 \" \\\"hello world\\\" 123\"): "
    P.++ s18

  let s19 = showParseResult $ parse firstLetter (loadStr32 "abc")
  putStrLn $ "parse firstLetter (loadStr32 \"abc\"): " P.++ s19

  let s20 = showParseResult $ parse firstLetter (loadStr32 "_abc")
  putStrLn $ "parse firstLetter (loadStr32 \"_abc\"): " P.++ s20

  let s21 = showParseResult $ parse charP (loadStr32 "'z' abc")
  putStrLn $ "parse charP (loadStr32 \"'z' abc\"): " P.++ s21

  let s22 = showParseResult $ parse charP (loadStr32 "abc")
  putStrLn $ "parse charP (loadStr32 \"abc\"): " P.++ s22

  let s23 = showParseResult $ parse numberP (loadStr32 "123 abc")
  putStrLn $ "parse numberP (loadStr32 \"123 abc\"): " P.++ s23

  let s24 = showParseResult $ parse numberP (loadStr32 "-123 abc")
  putStrLn $ "parse numberP (loadStr32 \"-123 abc\"): " P.++ s24

  let s24 = showParseResult $ parse numberP (loadStr32 "abc")
  putStrLn $ "parse numberP (loadStr32 \"abc\"): " P.++ s24

  let s25 = showParseResult $ parse quotedStringP (loadStr32 "\"abc\" 123")
  putStrLn $ "parse quotedStringP (loadStr32 \"\\\"abc\\\" 123\"): " P.++ s25

  let s26 = showParseResult $ parse word (loadStr32 "dup +")
  putStrLn $ "parse word (loadStr32 \"dup +\"): " P.++ s26

  let s27 = showParseResult $ parse lineComment (loadStr32 "$ zzz \n dup")
  putStrLn $ "parse lineComment (loadStr32 \"$ zzz \\n dup\"): " P.++ s27

  let s28 = showParseResult $ parse blockComment (loadStr32 "{ zzz } dup")
  putStrLn $ "parse blockComment (loadStr32 \"{ zzz } dup\"): " P.++ s28

  let s29 = showParseResult $ parse comment (loadStr32 "$ zzz \n dup")
  putStrLn $ "parse comment (loadStr32 \"$ zzz \\n dup\"): " P.++ s29

  let s30 = showParseResult $ parse comment (loadStr32 "{ zzz } dup")
  putStrLn $ "parse comment (loadStr32 \"{ zzz } dup\"): " P.++ s30

  let s31 = showParseResult $ parse comments (loadStr32 "$ a \n {z} {z} dup")
  putStrLn $ "parse comments (loadStr32 \"$ a \\n {z} {z} dup\"): " P.++ s31

  let s32 = showParseResult $ parse specification (loadStr32 "( X -> -- ) a")
  putStrLn $ "parse specification (loadStr32 \"( X -> -- ) a\"): " P.++ s32

  let s33 = showParseResult $ parse specifications (loadStr32 "(a) (b) a")
  putStrLn $ "parse specifications (loadStr32 \"(a) (b) a\"): " P.++ s33

  let s34 = showParseResult
        $ parse spacesCommentsSpecifications (loadStr32 " {a} (b) a")
  putStrLn
    $    "parse spacesCommentsSpecifications (loadStr32 \" {a} (b) a\"): "
    P.++ s34














































