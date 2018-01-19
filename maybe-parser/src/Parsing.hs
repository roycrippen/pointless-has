module Parsing where

import           Control.Monad (ap, liftM)
import           Data.Char

infixr 5 +++

newtype Parser a = Parser (String -> Maybe (a,String))

parse :: Parser a -> String -> Maybe (a,String)
parse (Parser p) = p

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap


instance Monad Parser where
  return v = Parser (\inp -> Just (v,inp))
  p >>= f  = Parser (\inp -> do  (v,out) <- parse p inp
                                 parse (f v) out)
--
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus Parser where
  mzero       = Parser (const Nothing)
  p `mplus` q = Parser 
    (\inp -> case parse p inp of
      Nothing -> parse q inp
      Just x  -> Just x
    )

failure :: Parser a
failure = mzero

item :: Parser Char
item = Parser 
  (\case
    []     -> Nothing
    (x:xs) -> Just (x,xs)
  )

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

sat   :: (Char -> Bool) -> Parser Char
sat p   = do x <- item
             if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char  :: Char -> Parser Char
char x  = sat (== x)

notChar :: Char -> Parser Char
notChar x = sat (/= x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many  :: Parser a -> Parser [a]
many p  = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat   :: Parser Int
nat   = do xs <- many1 digit
           return (read xs)

int   :: Parser Int
int   = do char '-'
           n <- nat
           return (-n)
           +++ nat

space   :: Parser ()
space   = do many (sat isSpace)
             return ()

token   :: Parser a -> Parser a
token p   = do space
               v <- p
               space
               return v

identifier    :: Parser String
identifier    = token ident

natural       :: Parser Int
natural   = token nat

integer       :: Parser Int
integer   = token int

symbol        :: String -> Parser String
symbol xs = token (string xs)

comment       :: Parser ()
comment   = do string "--"
               many $ notChar '\n'
               char '\n'
               return ()

-- formula evaluator
expr  :: Parser Int
expr  = do t <- term
           do symbol "+"
              e <- expr
              return (t + e)
              +++ do symbol "-"
                     e <- expr
                     return (t - e)
                     +++ return t

term  :: Parser Int
term  = do f <- factor
           do symbol "*"
              t <- term
              return (f * t)
              +++ do symbol "/"
                     t <- term
                     return (f `div` t)
                     +++ return f

factor  :: Parser Int
factor  = do symbol "("
             e <- expr
             symbol ")"
             return e
             +++ natural

eval  :: String -> Int
eval xs = case parse expr xs of
                Just (n,[]) -> n
                Nothing     -> error "invalid input"

