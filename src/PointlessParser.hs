module PointlessParser where

-- import           Debug.Trace
import           Interpreter
import           Parser                         ( Parser
                                                , anyChar
                                                , char
                                                , firstLetter
                                                , many
                                                , manyTill
                                                , newline
                                                , numberDouble
                                                , quotedString
                                                , spaces
                                                , string
                                                , wordLetter
                                                , (<|>)
                                                )

numberP :: Parser ValueP
numberP = NumP <$> numberDouble

charP :: Parser ValueP
charP = do
  _ <- char '\''
  c <- newline <|> firstLetter
  _ <- char '\''
  return (Chr c)

quotedStringP :: Parser ValueP
quotedStringP = Str <$> quotedString

word :: Parser ValueP
word = do
  c  <- firstLetter
  cs <- many wordLetter
  return (Symbol (c : cs))

instruction :: Parser ValueP
instruction = do
  _   <- spacesCommentsSpecifications
  res <- numberP <|> charP <|> quotedStringP <|> quotation <|> word
  _   <- spacesCommentsSpecifications
  return res

nakedQuotations :: Parser [ValueP]
nakedQuotations = many instruction

quotation :: Parser ValueP
quotation = do
  _ <- char '['
  _ <- spaces
  q <- nakedQuotations
  -- traceM $ "\nq: " ++ show q
  _ <- spaces
  _ <- char ']'
  return (Quot q)

lineComment :: Parser ()
lineComment = string "$" >> manyTill anyChar newline >> spaces >> return ()

blockComment :: Parser ()
blockComment =
  char '{' >> manyTill anyChar (char '}') >> char '}' >> spaces >> return ()

comment :: Parser ()
comment = lineComment <|> blockComment

comments :: Parser [()]
comments = many comment

specification :: Parser ()
specification =
  char '(' >> manyTill anyChar (char ')') >> char ')' >> spaces >> return ()

specifications :: Parser [()]
specifications = many specification

spacesCommentsSpecifications :: Parser ()
spacesCommentsSpecifications =
  spaces >> comments >> specifications >> return ()


-- parsers to get inline test from inside {}
lineComments :: Parser [()]
lineComments = many lineComment

spacesLineCommentsSpecifications :: Parser ()
spacesLineCommentsSpecifications =
  spaces >> lineComments >> specifications >> return ()

nonTest :: Parser ()
nonTest = do
  _ <- spacesLineCommentsSpecifications
  _ <- numberP <|> charP <|> quotedStringP <|> quotation <|> word
  _ <- spacesLineCommentsSpecifications
  return ()

nonTests :: Parser [()]
nonTests = many nonTest

testBlock :: Parser String
testBlock = do
  _ <- char '{'
  s <- manyTill anyChar (char '}')
  _ <- char '}'
  _ <- spaces
  return s

test :: Parser String
test = do
  _ <- many nonTest
  t <- testBlock
  _ <- many nonTest
  return t

tests :: Parser [String]
tests = many test


