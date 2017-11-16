module PointlessParser where

import           Interpreter (Stack, Value (..), WordP (..))
import           Parser      (Parser, anyChar, char, firstLetter, many,
                              manyTill, newline, numberDouble, spaces, string,
                              wordLetter, (<|>))

numberP :: Parser Value
numberP = do
    d <- numberDouble
    return (Number d)

word :: Parser Value
word = do
    c <- firstLetter
    cs <- many wordLetter
    return (Symbol (c:cs))

instruction :: Parser Value
instruction = do
    spaces
    result <- quotation <|> word <|> numberP
    spaces
    return result

nakedQuotations :: Parser Stack
nakedQuotations = many instruction

quotation :: Parser Value
quotation = do
    char '['
    q <- nakedQuotations
    char ']'
    return (Quot q)

definitionHeader :: Parser String
definitionHeader = do
    name <- word
    spaces
    string "=="
    spaces
    case name of
        Symbol x -> return x
        _        -> return "INVALID_DEFINITION_NAME"

definition :: Parser (String, WordP)
definition = do
    spaces
    comments
    string "DEFINE"
    spaces
    name <- definitionHeader
    q <- nakedQuotations
    spaces
    char ';'
    spaces
    comments
    return (name, Quotation q)

program :: Parser ([(String, WordP)], Stack)
program = do
    spaces
    comments
    ds <- many definition
    spaces
    comments
    qs <- nakedQuotations
    spaces
    comments
    return (ds, qs)

comment :: Parser ()
comment =
    (string "#" >> manyTill anyChar newline >> spaces >> return ()) <|>
    (string "(*" >> manyTill anyChar (string "*)") >> string "*)" >> spaces >> return ())

comments :: Parser [()]
comments = many comment

