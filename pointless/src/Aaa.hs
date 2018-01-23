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
import qualified Data.List     as L (foldl, length, repeat, reverse,
                               take, head, last, drop)
import           Data.Maybe    (fromJust, isJust)
import           Data.String   ()
import qualified Prelude       as P (replicate, (++))
import Interpreter (ValueP'(..))

import Debug.Trace

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
-- cntConsecutive :: ((1 <=? n) ~ True, KnownNat n, Num a) => Vec n Char -> a
cntConsecutive vs = case findIndex (=='~') vs of
  Just n -> fromIntegral (toInteger n)
  _      -> length vs
--
-- cntConsecutive :: KnownNat n => Vec n Char -> Int
-- cntConsecutive vs = go 0 vs vs
--  where
--   go :: KnownNat n => Int -> Vec n Char -> Vec n Char -> Int
--   go cnt orig new = do
--     let new' = rotateLeft new one
--     if (new !! 0) == '~' || new' == orig then cnt else go (cnt + 1) orig new'

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
vecToString vs = show $ L.reverse removeTilda
  where removeTilda = foldl (\acc c -> if c /= '~' then c : acc else acc) "" vs

showVec :: String -> String
showVec s = if L.length s > 1 && L.head s == '<' && L.last s == '>'
  then filter (\c -> c /= ',' && c /= '\'' && c /= '~') $ show s
  else case L.take 3 s of
    "Sym" -> "Sym' " P.++ showVec (L.drop 5 s)
    "Str" -> "Str' " P.++ showVec (L.drop 5 s)
    _     -> s

showParse :: Show a => Maybe (a, Vec n Char) -> String
showParse res = if isJust res
  then do
    let (r, vec) = fromJust res
        r'       = showVec $ show r
    "(" P.++ r' P.++ ", " P.++ vecToString vec P.++ ")"
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

  let s1 = parse (oneOf $ loadStr16 "cba") (loadStr32 "abc 123")
      r1 = "('a', \"bc 123\")"
  putStrLn $ "parse oneOf:             " P.++ show (r1 == showParse s1)

  let s2 = parse (string $ loadStr16 "abc") (loadStr32 "abc   123")
      r2 = "(\"<abc>\", \"   123\")"
  putStrLn $ "parse string:            " P.++ show (r2 == showParse s2)

  let s3 = parse spaces (loadStr32 "   123")
      r3 = "((), \"123\")"
  putStrLn $ "parse spaces:            " P.++ show (r3 == showParse s3)

  let s4 = parse digit (loadStr32 "123")
      r4 = "('1', \"23\")"
  putStrLn $ "parse digit:             " P.++ show (r4 == showParse s4)

  let s5 = parse numberInt (loadStr32 "123")
      r5 = "(123, \"\")"
  putStrLn $ "parse numberInt:         " P.++ show (r5 == showParse s5)

  let s6 = parse numberInt (loadStr32 "123 abc")
      r6 = "(123, \" abc\")"
  putStrLn $ "parse numberInt:         " P.++ show (r6 == showParse s6)

  let s7 = parse (oneOf $ loadStr16 "defa") (loadStr32 "abc   123")
      r7 = "('a', \"bc   123\")"
  putStrLn $ "parse oneOf:             " P.++ show (r7 == showParse s7)

  let s8 = parse (oneOf $ loadStr16 "cdef") (loadStr32 "abc   123")
      r8 = "Nothing"
  putStrLn $ "parse oneOf:             " P.++ show (r8 == showParse s8)

  let s9 = parse (noneOf $ loadStr16 "def") (loadStr32 "abc   123")
      r9 = "('a', \"bc   123\")"
  putStrLn $ "parse NoneOf:            " P.++ show (r9 == showParse s9)

  let s10 = parse (manyChar (char 'a')) (loadStr32 "aaa bbb")
      r10 = "(\"<aaa>\", \" bbb\")"
  putStrLn $ "parse manyChar:          " P.++ show (r10 == showParse s10)

  let s11 = parse (manyChar (char 'a')) (loadStr32 "a bbb")
      r11 = "(\"<a>\", \" bbb\")"
  putStrLn $ "parse manyChar:          " P.++ show (r11 == showParse s11)

  let s12 = parse (manyChar (char 'a')) (loadStr32 "bbb aaa")
      r12 = "(\"<>\", \"bbb aaa\")"      
  putStrLn $ "parse manyChar:          " P.++ show (r12 == showParse s12)

  let s13 = parse (many1Char (char 'a')) (loadStr32 "aaa bbb")
      r13 = "(\"<aaa>\", \" bbb\")"
  putStrLn $ "parse many1Char:         " P.++ show (r13 == showParse s13)

  let s14 = parse (many1Char (char 'a')) (loadStr32 "a bbb")
      r14 = "(\"<a>\", \" bbb\")"
  putStrLn $ "parse many1Char:         " P.++ show (r14 == showParse s14)

  let s15 = parse (many1Char (char 'a')) (loadStr32 "bbb aaa")
      r15 = "Nothing"
  putStrLn $ "parse many1Char:         " P.++ show (r15 == showParse s15)

  let s16 = parse emptyQuot (loadStr32 "[] abc")
      r16 = "(\"<[]>\", \" abc\")"
  putStrLn $ "parse emptyQuot:         " P.++ show (r16 == showParse s16)

  let s17 = parse (manyTillChar anyChar (char '}')) (loadStr32 "123} ccc")
      r17 = "(\"<123>\", \"} ccc\")"
  putStrLn $ "parse manyTillChar:      "  P.++ show (r17 == showParse s17)

  let s18 = parse quotedString (loadStr32 "\"hello world\" 123")
      r18 = "(\"<hello world>\", \" 123\")"
  putStrLn $ "parse quotedString:      " P.++ show (r18 == showParse s18)

  let s19 = parse firstLetter (loadStr32 "abc")
      r19 = "('a', \"bc\")"
  putStrLn $ "parse firstLetter:       " P.++ show (r19 == showParse s19)

  let s20 = parse firstLetter (loadStr32 "_abc")
      r20 = "('_', \"abc\")"
  putStrLn $ "parse firstLetter:       " P.++ show (r20 == showParse s20)

  let s21 = parse charP (loadStr32 "'z' abc")
      r21 = "(Chr' 'z', \" abc\")"
  putStrLn $ "parse charP:             " P.++ show (r21 == showParse s21)

  let s22 = parse charP (loadStr32 "abc")
      r22 = "Nothing"
  putStrLn $ "parse charP:             " P.++ show (r22 == showParse s22)

  let s23 = parse numberP (loadStr32 "123 abc")
      r23 = "(NumP' 123, \" abc\")"
  putStrLn $ "parse numberP:           " P.++ show (r23 == showParse s23)

  let s24 = parse numberP (loadStr32 "-123 abc")
      r24 = "(NumP' (-123), \" abc\")"
  putStrLn $ "parse numberP:           " P.++ show (r24 == showParse s24)

  let s25 = parse numberP (loadStr32 "abc")
      r25 = "Nothing"
  putStrLn $ "parse numberP:           " P.++ show (r25 == showParse s25)

  let s26 = parse quotedStringP (loadStr32 "\"abc\" 123")
      r26 = "(Str' \"<abc>\", \" 123\")"
  putStrLn $ "parse quotedStringP:     " P.++ show (r26 == showParse s26)

  let s27 = parse word (loadStr32 "dup +")
      r27 = "(Sym' \"<dup>\", \" +\")"
  putStrLn $ "parse word:              " P.++ show (r27 == showParse s27)

  let s28 = parse lineComment (loadStr32 "$ zzz \n dup")
      r28 = "((), \"dup\")"
  putStrLn $ "parse lineComment:       " P.++ show (r28 == showParse s28)

  let s29 = parse blockComment (loadStr32 "{ zzz } dup")
      r29 = "((), \"dup\")"
  putStrLn $ "parse blockComment:      " P.++ show (r29 == showParse s29)

  let s30 = parse comment (loadStr32 "$ zzz \n dup")
      r30 = "((), \"dup\")"
  putStrLn $ "parse comment:           " P.++ show (r30 == showParse s30)

  let s31 = parse comment (loadStr32 "{ zzz } dup")
      r31 = "((), \"dup\")"
  putStrLn $ "parse comment:           " P.++ show (r31 == showParse s31)

  let s32 = parse comments (loadStr32 "$ a \n {z} {z} dup")
      r32 = "((), \"dup\")"
  putStrLn $ "parse comments:          " P.++ show (r32 == showParse s32)

  let s33 = parse specification (loadStr32 "( X -> -- ) a")
      r33 = "((), \"a\")"
  putStrLn $ "parse specification:     " P.++ show (r33 == showParse s33)

  let s34 = parse specifications (loadStr32 "(a) (b) a")
      r34 = "((), \"a\")"
  putStrLn $ "parse specifications:    " P.++ show (r34 == showParse s34)

  let s35 = parse spacesCommentsSpecifications (loadStr32 " {a} (b) a")
      r35 = "((), \"a\")"
  putStrLn $ "parse spacesCommentsSpecifications: " P.++ show (r35 == showParse s35)
      















































































