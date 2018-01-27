{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
-- {-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- stack exec --resolver=nightly-2017-08-15 -- clash --interactive pointless/src/Aaa.hs

module Main where

import           Clash.Prelude         hiding ((<|>))
import           Clash.Promoted.Nat.TH

import           Control.Monad         (ap, liftM, void)
import qualified Data.Char             as C (digitToInt)
import qualified Data.List             as L (drop, foldl, head, last, length, repeat, reverse, take)
import           Data.Maybe            (fromJust, isJust)
import           Data.String           ()
import           Interpreter           (Q (..), V (..), ValueP' (..), cntConsecutive, pruneQ,
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
  )
--

-- | Parse a fixed length string.
-- | "abc" == <'a','b','c','~','~','~','~','~','~','~','~','~','~','~','~','~'>
string :: Vec 16 Char -> Parser (Vec 16 Char)
string s = Parser
  ( \vs -> do
    let vs' = pruneV vs
        p x = not (isStrMatch s (take d16 x))
        a = dropN (cntConsecutive '~' s) '~'
    case vs' of
      V1024 v -> if p v then Nothing else Just (s, V1024 (a v))
      V512  v -> if p v then Nothing else Just (s, V512 (a v))
      V256  v -> if p v then Nothing else Just (s, V256 (a v))
      V128  v -> if p v then Nothing else Just (s, V128 (a v))
      V64   v -> if p v then Nothing else Just (s, V64 (a v))
      V32   v -> if p v then Nothing else Just (s, V32 (a v))
      V16   v -> if p v then Nothing else Just (s, V16 (a v))
      V8    v -> if p v then Nothing else Just (s, V8 (a v))
      V4    v -> if p v then Nothing else Just (s, V4 (a v))
      V2    v -> if p v then Nothing else Just (s, V2 (a v))
  )
-- manyChar :: Parser Char -> Parser (Vec 64 Char)
-- manyChar p = Parser
--   ( \vs -> do
--     let vs' = map (parseChar p . repeat) vs
--         cnt = cntConsecutive '~' vs'
--         res = imap (\i x -> if fromIntegral i < cnt then x else '~') vs'
--     Just (res, dropN cnt '~' vs)
--   )

-- many1Char :: Parser Char -> Parser (Vec 64 Char)
-- many1Char p = do
--   a <- lookAhead p
--   if not a then failure else manyChar p

-- manyTillChar :: Parser Char -> Parser Char -> Parser (Vec 64 Char)
-- manyTillChar p end = Parser
--   ( \vs -> do
--     let vs'  = map (parseChar p . repeat) vs
--         vs'' = map (\x -> if x == parseChar end (repeat x) then '~' else x) vs'
--         cnt  = cntConsecutive '~' vs''
--         res  = imap (\i x -> if fromIntegral i < cnt then x else '~') vs'
--     Just (res, dropN cnt '~' vs)
--   )

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

-- -- | Lexical combinators
-- -- |
-- spaces :: Parser ()
-- spaces = void (manyChar (satisfies isSpace))
--  where
--   isSpace ' '  = True
--   isSpace '\n' = True
--   isSpace '\r' = True
--   isSpace '\t' = True
--   isSpace _    = False

-- digit :: Parser Char
-- digit = satisfies isDigit
--  where
--   isDigit c = isJust (findIndex (==c) digits)
--   digits =
--     '0' :> '1' :> '2' :> '3' :> '4' :> '5' :> '6' :> '7' :> '8' :> '9' :> Nil

-- numberInt :: Parser Int
-- numberInt = do
--   sign   <- char '-' <|> digit
--   digits <- many1Char digit
--   return (fromDigits (sign +>> digits))

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


-- letter :: Parser Char
-- letter = satisfies isAlpha where isAlpha c = isJust (findIndex (==c) letters)

-- azLower :: Vec 26 Char
-- azLower =
--   'a'
--     :> 'b'
--     :> 'c'
--     :> 'd'
--     :> 'e'
--     :> 'f'
--     :> 'g'
--     :> 'h'
--     :> 'i'
--     :> 'j'
--     :> 'k'
--     :> 'l'
--     :> 'm'
--     :> 'n'
--     :> 'o'
--     :> 'p'
--     :> 'q'
--     :> 'r'
--     :> 's'
--     :> 't'
--     :> 'u'
--     :> 'v'
--     :> 'w'
--     :> 'x'
--     :> 'y'
--     :> 'z'
--     :> Nil

-- azUpper :: Vec 26 Char
-- azUpper =
--   'A'
--     :> 'B'
--     :> 'C'
--     :> 'D'
--     :> 'E'
--     :> 'F'
--     :> 'G'
--     :> 'H'
--     :> 'I'
--     :> 'J'
--     :> 'K'
--     :> 'L'
--     :> 'M'
--     :> 'N'
--     :> 'O'
--     :> 'P'
--     :> 'Q'
--     :> 'R'
--     :> 'S'
--     :> 'T'
--     :> 'U'
--     :> 'V'
--     :> 'W'
--     :> 'X'
--     :> 'Y'
--     :> 'Z'
--     :> Nil

-- letters :: Vec 52 Char
-- letters = azLower ++ azUpper

-- firstLetter :: Parser Char
-- firstLetter = letter <|> oneOf symbols
--  where
--   symbols =
--     '+'
--       :> '-'
--       :> '*'
--       :> '/'
--       :> '<'
--       :> '>'
--       :> '='
--       :> '!'
--       :> '?'
--       :> '$'
--       :> '%'
--       :> '&'
--       :> '@'
--       :> '´'
--       :> '\''
--       :> '_'
--       :> Nil

-- wordLetter :: Parser Char
-- wordLetter = firstLetter <|> digit

-- newline :: Parser Char
-- newline = char '\n'

-- crlf :: Parser Char
-- crlf = char '\r' *> char '\n'

-- endOfLine :: Parser Char
-- endOfLine = newline <|> crlf

-- anyChar :: Parser Char
-- anyChar = satisfies (const True)

-- -- emptyQuot :: Parser (Vec 16 Char)
-- -- emptyQuot = string ('[' +>> ']' +>> blank16)

-- escapeNewLine :: Parser Char
-- escapeNewLine = do
--   b <- lookAhead (string ('\\' +>> '\n' +>> blank16))
--   if b
--     then do
--       _ <- char '\\'
--       char '\n'
--     else failure

-- nonEscape :: Parser Char
-- nonEscape = noneOf ('\\' +>> '\"' +>> blank16)

-- quotedString :: Parser (Vec 64 Char)
-- quotedString = do
--   char '"'
--   s <- manyChar (escapeNewLine <|> nonEscape)
--   char '"'
--   return s

-- -- | Pointless specific parsers
-- --
-- numberP :: Parser ValueP'
-- numberP = do
--   d <- numberInt
--   return (NumP' d)

-- charP :: Parser ValueP'
-- charP = do
--   _ <- char '\''
--   c <- newline <|> firstLetter
--   _ <- char '\''
--   return (Chr' c)

-- quotedStringP :: Parser ValueP'
-- quotedStringP = do
--   str <- quotedString
--   return (Str' (take d32 str))

-- word :: Parser ValueP'
-- word = do
--   c  <- firstLetter
--   cs <- manyChar wordLetter
--   return (Sym' (select d0 d1 d16 (c +>> cs)))

-- lineComment :: Parser ()
-- lineComment = char '$' >> manyTillChar anyChar newline >> spaces >> return ()

-- blockComment :: Parser ()
-- blockComment =
--   char '{' >> manyTillChar anyChar (char '}') >> char '}' >> spaces >> return ()

-- comment :: Parser ()
-- comment = lineComment <|> blockComment

-- manyEmpty :: Parser () -> Parser ()
-- manyEmpty p = do
--   com <- lookAhead p
--   if com
--     then do
--       _ <- p
--       _ <- manyEmpty p
--       return ()
--     else return ()

-- comments :: Parser ()
-- comments = manyEmpty comment

-- specification :: Parser ()
-- specification =
--   char '(' >> manyTillChar anyChar (char ')') >> char ')' >> spaces >> return ()

-- specifications :: Parser ()
-- specifications = manyEmpty specification

-- spacesCommentsSpecifications :: Parser ()
-- spacesCommentsSpecifications = spaces >> comments >> specifications >> comments

-- -- |parsers to get inline test from inside {}
-- lineComments :: Parser ()
-- lineComments = manyEmpty lineComment

-- spacesLineCommentsSpecifications :: Parser ()
-- spacesLineCommentsSpecifications =
--   spaces >> lineComments >> specifications >> lineComments
-- --
-- parseValueP :: Parser ValueP' -> Vec 64 Char -> ValueP'
-- parseValueP p vs = case parse p vs of
--   Just (p, _) -> p
--   Nothing     -> EmptyQ

-- manyQ :: Parser ValueP' -> Parser Q
-- manyQ p = Parser
--   ( \vs -> case parseValueP p vs of
--     EmptyQ -> Nothing
--     _      -> Just (mP p vs)
--   )

-- mP :: Parser ValueP' -> Vec 64 Char -> (Q, Vec 64 Char)
-- mP p_ vs_ = (pruneQ (Q1024 resQ'), resS)
--  where
--   (resQ, resS) = go p_ vs_ (repeat EmptyQ :: Vec 1024 ValueP')
--   cnt          = foldl (\acc v -> if v == EmptyQ then acc + 1 else acc) 0 resQ
--   resQ'        = rotateLeft resQ cnt
--   go p vs qs = case parseValueP p vs of
--     EmptyQ -> (qs, vs)
--     _      -> do
--       let (vp, vs') = fromJust $ parse p vs
--       go p vs' (qs <<+ vp)


-- instruction :: Parser ValueP'
-- instruction = do
--   _   <- spacesCommentsSpecifications
--   res <- numberP <|> charP <|> quotedStringP <|> quotation <|> word
--   _   <- spacesCommentsSpecifications
--   return res

-- nakedQuotations :: Parser Q
-- nakedQuotations = manyQ instruction

-- quotation :: Parser ValueP'
-- quotation = do
--   _ <- char '['
--   _ <- spaces
--   q <- nakedQuotations
--   -- traceM $ "\nq: " ++ show q
--   _ <- spaces
--   _ <- char ']'
--   return (Quot' q)

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

-- parseChar :: Parser Char -> Vec 64 Char -> Char
-- parseChar p vs = case parse p vs of
--   Just (c, _) -> c
--   Nothing     -> '~'

-- | dropN n chars from vs
dropN :: KnownNat n => Int -> a -> Vec n a -> Vec n a
dropN 0   _ vs = vs
dropN cnt c vs = dropN (cnt - 1) c (vs <<+ c)

-- -- | Count non '~' consecutive charaters starting a Vector
-- cntConsecutive :: (Eq a, KnownNat n) => a -> Vec n a -> Int
-- cntConsecutive a vs = case findIndex (==a) vs of
--   Just n -> fromIntegral (toInteger n)
--   _      -> length vs

-- -- | Covert a vector of chars to an int
-- -- | takes chars != '~'
-- fromDigits :: KnownNat n => Vec n Char -> Int
-- fromDigits vs = val * sign
--  where
--   isMinus = vs !! (0 :: Integer) == '-'
--   vs'     = if isMinus then vs <<+ '~' else vs
--   sign    = if isMinus then (-1) else 1
--   val =
--     foldl (\acc c -> if c /= '~' then 10 * acc + C.digitToInt c else acc) 0 vs'


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
  then filter (\c -> c /= ',' && c /= '\'' && c /= '~') $ show s
  else case L.take 3 s of
    "Sym" -> "Sym' " P.++ showVec (L.drop 5 s)
    "Str" -> "Str' " P.++ showVec (L.drop 5 s)
    _     -> s

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

-- blank16 :: Vec 16 Char
-- blank16 = repeat '~'

blank64 :: Vec 64 Char
blank64 = repeat '~'

blank1024 :: Vec 1024 Char
blank1024 = repeat '~'

blank65536 :: Vec 65536 Char
blank65536 = repeat '~'

-- -- (qs, _) = fromJust $ parse nakedQuotations  (loadStr64 d64 "[10 20 +] dup exec swap exec + [10 20 +] dup exec swap exec +")

-- -- | Parser tests.
-- -- |

main :: IO ()
main = do
  putStrLn "Pointless in Clash tests\n"
  putStrLn ""
  parserTests

  putStrLn $ show $ parse item (loadStr "abcl")

parserTests :: IO ()
parserTests = do
  putStrLn "parserTests..."

  let s1 = parse (oneOf $ loadStr64 d16 "cba") (loadStr "abc 123")
      r1 = "('a', \"bc 123\")"
  putStr $ "parse oneOf:             " P.++ show (r1 == showParse s1)
  putStrLn $ ",  result = " P.++ r1

--   let s2 = parse (string $ loadStr64 d16 "abc") (loadStr64 d64 "abc   123")
--       r2 = "(\"<abc>\", \"   123\")"
--   putStr $ "parse string:            " P.++ show (r2 == showParse s2)
--   putStrLn $ ",  result = " P.++ r2

--   let s3 = parse spaces (loadStr64 d64 "   123")
--       r3 = "((), \"123\")"
--   putStr $ "parse spaces:            " P.++ show (r3 == showParse s3)
--   putStrLn $ ",  result = " P.++ r3

--   let s4 = parse digit (loadStr64 d64 "123")
--       r4 = "('1', \"23\")"
--   putStr $ "parse digit:             " P.++ show (r4 == showParse s4)
--   putStrLn $ ",  result = " P.++ r4

--   let s5 = parse numberInt (loadStr64 d64 "123")
--       r5 = "(123, \"\")"
--   putStr $ "parse numberInt:         " P.++ show (r5 == showParse s5)
--   putStrLn $ ",  result = " P.++ r5

--   let s6 = parse numberInt (loadStr64 d64 "123 abc")
--       r6 = "(123, \" abc\")"
--   putStr $ "parse numberInt:         " P.++ show (r6 == showParse s6)
--   putStrLn $ ",  result = " P.++ r6

  let s7 = parse (oneOf $ loadStr64 d16 "defa") (loadStr "abc   123")
      r7 = "('a', \"bc   123\")"
  putStr $ "parse oneOf:             " P.++ show (r7 == showParse s7)
  putStrLn $ ",  result = " P.++ r7

  let s8 = parse (oneOf $ loadStr64 d16 "cdef") (loadStr "abc   123")
      r8 = "Nothing"
  putStr $ "parse oneOf:             " P.++ show (r8 == showParse s8)
  putStrLn $ ",  result = " P.++ r8

  let s9 = parse (noneOf $ loadStr64 d16 "def") (loadStr "abc   123")
      r9 = "('a', \"bc   123\")"
  putStr $ "parse NoneOf:            " P.++ show (r9 == showParse s9)
  putStrLn $ ",  result = " P.++ r9

--   let s10 = parse (manyChar (char 'a')) (loadStr64 d64 "aaa bbb")
--       r10 = "(\"<aaa>\", \" bbb\")"
--   putStr $ "parse manyChar:          " P.++ show (r10 == showParse s10)
--   putStrLn $ ",  result = " P.++ r10

--   let s11 = parse (manyChar (char 'a')) (loadStr64 d64 "a bbb")
--       r11 = "(\"<a>\", \" bbb\")"
--   putStr $ "parse manyChar:          " P.++ show (r11 == showParse s11)
--   putStrLn $ ",  result = " P.++ r11

--   let s12 = parse (manyChar (char 'a')) (loadStr64 d64 "bbb aaa")
--       r12 = "(\"<>\", \"bbb aaa\")"
--   putStr $ "parse manyChar:          " P.++ show (r12 == showParse s12)
--   putStrLn $ ",  result = " P.++ r12

--   let s13 = parse (many1Char (char 'a')) (loadStr64 d64 "aaa bbb")
--       r13 = "(\"<aaa>\", \" bbb\")"
--   putStr $ "parse many1Char:         " P.++ show (r13 == showParse s13)
--   putStrLn $ ",  result = " P.++ r13

--   let s14 = parse (many1Char (char 'a')) (loadStr64 d64 "a bbb")
--       r14 = "(\"<a>\", \" bbb\")"
--   putStr $ "parse many1Char:         " P.++ show (r14 == showParse s14)
--   putStrLn $ ",  result = " P.++ r14

--   let s15 = parse (many1Char (char 'a')) (loadStr64 d64 "bbb aaa")
--       r15 = "Nothing"
--   putStr $ "parse many1Char:         " P.++ show (r15 == showParse s15)
--   putStrLn $ ",  result = " P.++ r15

--   let s16 = parse (manyTillChar anyChar (char '}')) (loadStr64 d64 "123 ccc")
--       r16 = "(\"<123 ccc>\", \"\")"
--   putStr $ "parse manyTillChar:      " P.++ show (r16 == showParse s16)
--   putStrLn $ ",  result = " P.++ r16

--   let s17 = parse (manyTillChar anyChar (char '}')) (loadStr64 d64 "123} ccc")
--       r17 = "(\"<123>\", \"} ccc\")"
--   putStr $ "parse manyTillChar:      " P.++ show (r17 == showParse s17)
--   putStrLn $ ",  result = " P.++ r17

--   let s18 = parse quotedString (loadStr64 d64 "\"hello world\" 123")
--       r18 = "(\"<hello world>\", \" 123\")"
--   putStr $ "parse quotedString:      " P.++ show (r18 == showParse s18)
--   putStrLn $ ",  result = " P.++ r18

--   let s19 = parse firstLetter (loadStr64 d64 "abc")
--       r19 = "('a', \"bc\")"
--   putStr $ "parse firstLetter:       " P.++ show (r19 == showParse s19)
--   putStrLn $ ",  result = " P.++ r19

--   let s20 = parse firstLetter (loadStr64 d64 "_abc")
--       r20 = "('_', \"abc\")"
--   putStr $ "parse firstLetter:       " P.++ show (r20 == showParse s20)
--   putStrLn $ ",  result = " P.++ r20

--   let s21 = parse charP (loadStr64 d64 "'z' abc")
--       r21 = "(Chr' 'z', \" abc\")"
--   putStr $ "parse charP:             " P.++ show (r21 == showParse s21)
--   putStrLn $ ",  result = " P.++ r21

--   let s22 = parse charP (loadStr64 d64 "abc")
--       r22 = "Nothing"
--   putStr $ "parse charP:             " P.++ show (r22 == showParse s22)
--   putStrLn $ ",  result = " P.++ r22

--   let s23 = parse numberP (loadStr64 d64 "123 abc")
--       r23 = "(NumP' 123, \" abc\")"
--   putStr $ "parse numberP:           " P.++ show (r23 == showParse s23)
--   putStrLn $ ",  result = " P.++ r23

--   let s24 = parse numberP (loadStr64 d64 "-123 abc")
--       r24 = "(NumP' (-123), \" abc\")"
--   putStr $ "parse numberP:           " P.++ show (r24 == showParse s24)
--   putStrLn $ ",  result = " P.++ r24

--   let s25 = parse numberP (loadStr64 d64 "abc")
--       r25 = "Nothing"
--   putStr $ "parse numberP:           " P.++ show (r25 == showParse s25)
--   putStrLn $ ",  result = " P.++ r25

--   let s26 = parse quotedStringP (loadStr64 d64 "\"abc\" 123")
--       r26 = "(Str' \"<abc>\", \" 123\")"
--   putStr $ "parse quotedStringP:     " P.++ show (r26 == showParse s26)
--   putStrLn $ ",  result = " P.++ r26

--   let s27 = parse word (loadStr64 d64 "dup +")
--       r27 = "(Sym' \"<dup>\", \" +\")"
--   putStr $ "parse word:              " P.++ show (r27 == showParse s27)
--   putStrLn $ ",  result = " P.++ r27

--   let s28 = parse lineComment (loadStr64 d64 "$ zzz \n dup")
--       r28 = "((), \"dup\")"
--   putStr $ "parse lineComment:       " P.++ show (r28 == showParse s28)
--   putStrLn $ ",  result = " P.++ r28

--   let s29 = parse blockComment (loadStr64 d64 "{ zzz } dup")
--       r29 = "((), \"dup\")"
--   putStr $ "parse blockComment:      " P.++ show (r29 == showParse s29)
--   putStrLn $ ",  result = " P.++ r29

--   let s30 = parse comment (loadStr64 d64 "$ zzz \n dup")
--       r30 = "((), \"dup\")"
--   putStr $ "parse comment:           " P.++ show (r30 == showParse s30)
--   putStrLn $ ",  result = " P.++ r30

--   let s31 = parse comment (loadStr64 d64 "{ zzz } dup")
--       r31 = "((), \"dup\")"
--   putStr $ "parse comment:           " P.++ show (r31 == showParse s31)
--   putStrLn $ ",  result = " P.++ r31

--   let s32 = parse comments (loadStr64 d64 "$ a \n {z} {z} dup")
--       r32 = "((), \"dup\")"
--   putStr $ "parse comments:          " P.++ show (r32 == showParse s32)
--   putStrLn $ ",  result = " P.++ r32

--   let s33 = parse specification (loadStr64 d64 "( X -> -- ) a")
--       r33 = "((), \"a\")"
--   putStr $ "parse specification:     " P.++ show (r33 == showParse s33)
--   putStrLn $ ",  result = " P.++ r33

--   let s34 = parse specifications (loadStr64 d64 "(a) (b) a")
--       r34 = "((), \"a\")"
--   putStr $ "parse specifications:    " P.++ show (r34 == showParse s34)
--   putStrLn $ ",  result = " P.++ r34

--   let s35 = parse spacesCommentsSpecifications (loadStr64 d64 " {a} (b) a")
--       r35 = "((), \"a\")"
--   putStr $ "parse spacesCommentsSpecifications: " P.++ show
--     (r35 == showParse s35)
--   putStrLn $ ",  result = " P.++ r35

--   let xs =
--         NumP' 1
--           :> Chr' 'a'
--           :> Sym' (loadStr64 d16 "pop")
--           :> Str' (loadStr64 d32 "hello world")
--           :> EmptyQ
--           :> EmptyQ
--           :> EmptyQ
--           :> EmptyQ
--           :> EmptyQ
--           :> Nil
--   putStrLn $ show xs












