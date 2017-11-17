module PointlessParser where

import           Interpreter (Stack, Value (..), WordP (..))
import           Parser      (Parser, anyChar, char, firstLetter, many, manyTill, newline, numberDouble, spaces, string,
                              wordLetter, (<|>))

numberP :: Parser Value
numberP = do
    d <- numberDouble
    return (Number d)

word :: Parser Value
word = do
    c  <- firstLetter
    cs <- many wordLetter
    return (Symbol (c : cs))

instruction :: Parser Value
instruction = do
    _      <- spaces
    result <- quotation <|> word <|> numberP
    _      <- spaces
    return result

nakedQuotations :: Parser Stack
nakedQuotations = many instruction

quotation :: Parser Value
quotation = do
    _ <- char '['
    q <- nakedQuotations
    _ <- char ']'
    return (Quot q)

definitionHeader :: Parser String
definitionHeader = do
    name <- word
    _    <- spaces
    _    <- string "=="
    _    <- spaces
    case name of
        Symbol x -> return x
        _        -> return "INVALID_DEFINITION_NAME"

definition :: Parser (String, WordP)
definition = do
    _    <- spacesAndComments
    _    <- string "DEFINE"
    _    <- spaces
    name <- definitionHeader
    q    <- nakedQuotations
    _    <- spaces
    _    <- char ';'
    _    <- spacesAndComments
    return (name, Quotation q)

program :: Parser ([(String, WordP)], Stack)
program = do
    _  <- spacesAndComments
    ds <- many definition
    _  <- spaces
    _  <- comments
    qs <- nakedQuotations
    _  <- spacesAndComments
    return (ds, qs)

comment :: Parser ()
comment =
    (string "#" >> manyTill anyChar newline >> spaces >> return ())
        <|> (  string "(*"
            >> manyTill anyChar (string "*)")
            >> string "*)"
            >> spaces
            >> return ()
            )

comments :: Parser [()]
comments = many comment

spacesAndComments :: Parser ()
spacesAndComments = spaces >> comments >> return ()




