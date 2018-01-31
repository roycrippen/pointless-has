{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}

module Parser where

import qualified Data.Char as C (digitToInt)
import qualified Prelude   as P ()

import CLaSH.Prelude hiding ((<|>))
import Control.Monad (ap, liftM, void)
import Data.Maybe    (fromJust, isJust, isNothing)
import Interpreter   (Q (..), V (..), ValueP (..), lengthElem, pruneQ, pruneV)

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
      V65536 v -> if p v then Nothing else Just (v !! zero, V65536 (a v))
      V32768 v -> if p v then Nothing else Just (v !! zero, V32768 (a v))
      V16384 v -> if p v then Nothing else Just (v !! zero, V16384 (a v))
      V8192  v -> if p v then Nothing else Just (v !! zero, V8192 (a v))
      V4096  v -> if p v then Nothing else Just (v !! zero, V4096 (a v))
      V2048  v -> if p v then Nothing else Just (v !! zero, V2048 (a v))
      V1024  v -> if p v then Nothing else Just (v !! zero, V1024 (a v))
      V512   v -> if p v then Nothing else Just (v !! zero, V512 (a v))
      V256   v -> if p v then Nothing else Just (v !! zero, V256 (a v))
      V128   v -> if p v then Nothing else Just (v !! zero, V128 (a v))
      V64    v -> if p v then Nothing else Just (v !! zero, V64 (a v))
      V32    v -> if p v then Nothing else Just (v !! zero, V32 (a v))
      V16    v -> if p v then Nothing else Just (v !! zero, V16 (a v))
      V8     v -> if p v then Nothing else Just (v !! zero, V8 (a v))
      V4     v -> if p v then Nothing else Just (v !! zero, V4 (a v))
      V2     v -> if p v then Nothing else Just (v !! zero, V2 (a v))
  )

-- | String s matches.
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
      V65536 v -> if p v then Nothing else Just (s, pruneV $ V65536 (f v))
      V32768 v -> if p v then Nothing else Just (s, pruneV $ V32768 (f v))
      V16384 v -> if p v then Nothing else Just (s, pruneV $ V16384 (f v))
      V8192  v -> if p v then Nothing else Just (s, pruneV $ V8192 (f v))
      V4096  v -> if p v then Nothing else Just (s, pruneV $ V4096 (f v))
      V2048  v -> if p v then Nothing else Just (s, pruneV $ V2048 (f v))
      V1024  v -> if p v then Nothing else Just (s, pruneV $ V1024 (f v))
      V512   v -> if p v then Nothing else Just (s, pruneV $ V512 (f v))
      V256   v -> if p v then Nothing else Just (s, pruneV $ V256 (f v))
      V128   v -> if p v then Nothing else Just (s, pruneV $ V128 (f v))
      V64    v -> if p v then Nothing else Just (s, pruneV $ V64 (f v))
      V32    v -> if p v then Nothing else Just (s, pruneV $ V32 (f v))
      V16    v -> if p v then Nothing else Just (s, pruneV $ V16 (f v))
      _        -> Nothing
  )

-- | Parse many characters matching p.
-- |
-- | Same as manyTillChar except for mapParser.
-- | For manyChar -> mapParser = map (parseChar p)
manyChar :: Parser Char -> Parser V
manyChar p = manyTillChar p (char '~')

many1Char :: Parser Char -> Parser V
many1Char p = do
  a <- lookAhead p
  if not a then failure else manyChar p

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
      V65536 v -> Just (pruneV $ V65536 (left v), pruneV $ V65536 (right v))
      V32768 v -> Just (pruneV $ V32768 (left v), pruneV $ V32768 (right v))
      V16384 v -> Just (pruneV $ V16384 (left v), pruneV $ V16384 (right v))
      V8192  v -> Just (pruneV $ V8192 (left v), pruneV $ V8192 (right v))
      V4096  v -> Just (pruneV $ V4096 (left v), pruneV $ V4096 (right v))
      V2048  v -> Just (pruneV $ V2048 (left v), pruneV $ V2048 (right v))
      V1024  v -> Just (pruneV $ V1024 (left v), pruneV $ V1024 (right v))
      V512   v -> Just (pruneV $ V512 (left v), pruneV $ V512 (right v))
      V256   v -> Just (pruneV $ V256 (left v), pruneV $ V256 (right v))
      V128   v -> Just (pruneV $ V128 (left v), pruneV $ V128 (right v))
      V64    v -> Just (pruneV $ V64 (left v), pruneV $ V64 (right v))
      V32    v -> Just (pruneV $ V32 (left v), pruneV $ V32 (right v))
      V16    v -> Just (pruneV $ V16 (left v), pruneV $ V16 (right v))
      V8     v -> Just (pruneV $ V8 (left v), pruneV $ V8 (right v))
      V4     v -> Just (pruneV $ V4 (left v), pruneV $ V4 (right v))
      V2     v -> Just (pruneV $ V2 (left v), pruneV $ V2 (right v))
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
  isDigit c = isJust (elemIndex c digits)
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

letter :: Parser Char
letter = satisfies isAlpha where isAlpha c = isJust (elemIndex c letters)

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
numberP :: Parser ValueP
numberP = do
  d <- numberInt
  return (NumP d)

charP :: Parser ValueP
charP = do
  _ <- char '\''
  c <- newline <|> firstLetter
  _ <- char '\''
  return (Chr c)

quotedStringP :: Parser ValueP
quotedStringP = do
  str <- quotedString
  case str of
    V2    v -> return (Str (V2 v))
    V4    v -> return (Str (V4 v))
    V8    v -> return (Str (V8 v))
    V16   v -> return (Str (V16 v))
    V32   v -> return (Str (V32 v))
    V64   v -> return (Str (V64 v))
    V128  v -> return (Str (V128 v))
    V256  v -> return (Str (V256 v))
    V512  v -> return (Str (V512 v))
    V1024 v -> return (Str (V1024 v))
    _       -> failure

word :: Parser ValueP
word = do
  c  <- firstLetter
  cs <- manyChar wordLetter
  case cs of
    V2  v -> return (Sym (c :> select d0 d1 d2 v ++ drop d3 blank16))
    V4  v -> return (Sym (c :> select d0 d1 d4 v ++ drop d5 blank16))
    V8  v -> return (Sym (c :> select d0 d1 d8 v ++ drop d9 blank16))
    V16 v -> return (Sym (select d0 d1 d16 (c +>> v)))
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
parseValueP :: Parser ValueP -> V -> ValueP
parseValueP p vs = case parse p vs of
  Just (p, _) -> p
  Nothing     -> EmptyQ

manyQ :: Parser ValueP -> Parser Q
manyQ p = Parser
  ( \vs -> case parseValueP p vs of
    EmptyQ -> Nothing
    _      -> Just (mP p vs)
  )

mP :: Parser ValueP -> V -> (Q, V)
mP p_ vs_ = (pruneQ (Q64 resQ'), resS)
 where
  (resQ, resS) = go p_ vs_ (repeat EmptyQ :: Vec 64 ValueP)
  cnt          = foldl (\acc v -> if v == EmptyQ then acc + 1 else acc) 0 resQ
  resQ'        = rotateLeft resQ cnt
  go p vs qs = case parseValueP p vs of
    EmptyQ -> (qs, vs)
    _      -> do
      let (vp, vs') = fromJust $ parse p vs
      go p vs' (qs <<+ vp)

emptyQuot :: Parser ValueP
emptyQuot = do
  _ <- char '['
  _ <- spaces
  _ <- char ']'
  return (Quot (Q2 (EmptyQ :> EmptyQ :> Nil)))

instruction :: Parser ValueP
instruction = do
  _   <- spacesCommentsSpecifications
  res <-
    numberP <|> charP <|> quotedStringP <|> emptyQuot <|> quotation <|> word
  _ <- spacesCommentsSpecifications
  return res

nakedQuotations :: Parser Q
nakedQuotations = manyQ instruction

quotation :: Parser ValueP
quotation = do
  _ <- char '['
  _ <- spaces
  q <- nakedQuotations
  _ <- spaces
  _ <- char ']'
  return (Quot q)

-- nonTest :: Parser ()
-- nonTest = do
--   _ <- spacesLineCommentsSpecifications
--   _ <- numberP <|> charP <|> quotedStringP <|> quotation <|> word
--   _ <- spacesLineCommentsSpecifications
--   return ()

-- -- nonTests :: Parser [()]
-- nonTests = manyChar nonTest

-- testBlock :: Parser V
-- testBlock = do
--   _ <- char '{'
--   s <- manyTillChar anyChar (char '}')
--   _ <- char '}'
--   _ <- spaces
--   return s

-- test :: Parser V
-- test = do
--   _ <- many nonTest
--   t <- testBlock
--   _ <- many nonTest
--   return t

-- -- tests :: Parser [String]
-- tests = manyChar test


-- | Helper functions.
-- |
-- |
-- | Compare two vector strings.
-- | <a,b,c,~...> == <a,b,c,d...>
isStrMatch :: Vec 16 Char -> Vec 16 Char -> Bool
isStrMatch xs vs = fold (&&) zipped
  where zipped = zipWith (\x v -> x == v || x == '~') xs vs

-- | Get the character by applying parser p or '~' if Nothing.
parseChar :: Parser Char -> Char -> Char
parseChar p c = case parse p vs of
  Just (x, _) -> x
  Nothing     -> '~'
  where vs = V2 (c :> c :> Nil)

-- | popN n chars from vs
popN :: KnownNat n => Int -> a -> Vec n a -> Vec n a
popN 0   _ vs = vs
popN cnt c vs = popN (cnt - 1) c (vs <<+ c)

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

-- | Create empty string of length 16
blank16 :: Vec 16 Char
blank16 = repeat '~'

-- | Create empty string of length 64
blank64 :: Vec 64 Char
blank64 = repeat '~'

-- | Create empty string of length 1024
blank1024 :: Vec 1024 Char
blank1024 = repeat '~'

-- | Create empty string of length 65536
blank65536 :: Vec 65536 Char
blank65536 = repeat '~'






