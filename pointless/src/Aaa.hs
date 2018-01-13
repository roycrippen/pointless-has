{-# LANGUAGE DataKinds #-}

module Main where

import           Clash.Prelude
-- import qualified Data.Foldable as F
import           Data.String
-- import qualified Data.Text     as T
-- import qualified GHC.Base      as B
import           Control.Monad (ap, liftM)
import qualified Prelude       as P hiding (Monad)

newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser t -> String -> Maybe (t, String)
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
--
failure :: Parser a
failure = mzero

item :: Parser Char
item = Parser item'
 where
  item' s = case s of
    ""     -> Nothing
    (c:cs) -> Just (c, cs)


-- option :: Parser a -> Parser a -> Parser a
-- option p q = Parser
--   ( \s -> case parse (mplus p q) s of
--     []    -> []
--     (x:_) -> [x]
--   )

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = p `mplus` q

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = item >>= \c -> if p c then return c else failure

char :: Char -> Parser Char
char c = satisfies (c ==)

string :: String -> Parser String
string ""     = return ""
string (c:cs) = do
  _ <- char c
  _ <- string cs
  return (c : cs)

-- many :: Parser a -> Parser [a]
-- many p = many1 p <|> return []

-- many1 :: Parser a -> Parser [a]
-- many1 p = do
--   a  <- p
--   as <- many p
--   return (a : as)

-- sepBy :: Parser a -> Parser b -> Parser [a]
-- p `sepBy` sep = (p `sepBy1` sep) <|> return []

-- sepBy1 :: Parser a -> Parser b -> Parser [a]
-- p `sepBy1` sep = do
--   a  <- p
--   as <- many
--     ( do
--       _ <- sep
--       p
--     )
--   return (a : as)

-- chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
-- chainl p op a = (p `chainl1` op) <|> return a

-- chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- p `chainl1` op = do
--   a <- p
--   rest a
--  where
--   rest a =
--     ( do
--         f <- op
--         b <- p
--         rest (f a b)
--       )
--       <|> return a

-- oneOf :: String -> Parser Char
-- oneOf cs = satisfies (`elem` cs)

-- noneOf :: String -> Parser Char
-- noneOf cs = satisfies (`notElem` cs)

-- manyN :: Parser a -> Int -> Parser [a]
-- manyN p 1 = do
--   c <- p
--   return [c]
-- manyN p n = do
--   c    <- p
--   rest <- manyN p (n - 1)
--   return (c : rest)

-- manyTill :: Parser a -> Parser b -> Parser [a]
-- manyTill p end = manyTill1 p end <|> return []

-- manyTill1 :: Parser a -> Parser b -> Parser [a]
-- manyTill1 p end = do
--   a <- p
--   b <- lookAhead end
--   if b
--     then return [a]
--     else do
--       as <- manyTill p end
--       return (a : as)

-- lookAhead :: Parser a -> Parser Bool
-- lookAhead p = Parser
--   ( \s -> case parse p s of
--     [] -> [(False, s)]
--     _  -> [(True, s)]
--   )

-- -- | Lexical combinators
-- -- |
-- spaces :: Parser ()
-- spaces = void (many (satisfies isSpace))
--  where
--   isSpace ' '  = True
--   isSpace '\n' = True
--   isSpace '\r' = True
--   isSpace '\t' = True
--   isSpace _    = False

-- token :: Parser a -> Parser a
-- token p = do
--   _ <- spaces
--   a <- p
--   _ <- spaces
--   return a

-- symb :: String -> Parser String
-- symb s = token $ string s

-- digit :: Parser Char
-- digit = satisfies isDigit where isDigit c = isJust (find (== c) ['0' .. '9'])

-- numberInt :: Parser Int
-- numberInt = do
--   sign   <- string "-" <|> string ""
--   digits <- many1 digit
--   return (read (sign ++ digits) :: Int)

-- -- numberDouble :: Parser Double
-- -- numberDouble = do
-- --   sign     <- string "-" <|> string ""
-- --   digits   <- many1 digit
-- --   _        <- string "." <|> string ""
-- --   mantissa <- many digit
-- --   _        <- spaces
-- --   let mantissa' = if mantissa == "" then "0" else mantissa
-- --       double    = sign ++ digits ++ "." ++ mantissa'
-- --   return (read double :: Double)

-- letter :: Parser Char
-- letter = satisfies isAlpha
--  where
--   isAlpha c = isJust (find (== c) letters)
--   letters = ['a' .. 'z'] ++ ['A' .. 'Z']

-- firstLetter :: Parser Char
-- firstLetter = letter <|> oneOf "+-*/<>=!?§$%&@~´',:._"

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


main :: IO ()
main = do
  putStrLn "Pointless in Clash tests\n"
  parserTests

parserTests :: IO ()
parserTests = putStrLn "parserTests..."
  -- let (a1, _):_ = parse numberInt "123 abc"
  -- putStrLn $  "parse numberInt: " ++ if (a1 == 123) then "OK" else "ERROR"
  -- putStrLn $ show a1 ++ "\n"

  -- let (s1, _):_ = parse quotedString "\"hello world\" 123"
  -- putStrLn $ "parse quotedString: " ++ if (s1 == "hello world") then "OK" else "ERROR"
  -- putStrLn $ s1 ++ "\n"

  -- let (c1, _):_ = parse firstLetter "abc"
  -- putStrLn $ "parse firstLetter: " ++ if (c1 == 'a') then "OK" else "ERROR"
  -- putStrLn $ show c1 ++ "\n"

  -- let (c2, _):_ = parse firstLetter "_abc"
  -- putStrLn $ "parse firstLetter: " ++ if (c2 == '_') then "OK" else "ERROR"
  -- putStrLn $ show c2 ++ "\n"

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




