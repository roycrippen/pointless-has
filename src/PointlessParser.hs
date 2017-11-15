module PointlessParser where

import           Interpreter (Stack, Value (..), WordP (..))
import           Parser      (Parser, char, firstLetter, many, numberDouble,
                              spaces, string, wordLetter, (<|>))

numberP :: Parser Value
numberP = do
    d <- numberDouble
    return (Number d)

word :: Parser Value
word = do
    spaces
    c <- firstLetter
    cs <- many wordLetter
    spaces
    return (Symbol (c:cs))

instruction :: Parser Value
instruction = quotation <|> word <|> numberP

nakedQuotations :: Parser Stack
nakedQuotations = many instruction

quotation :: Parser Value
quotation = do
    spaces
    char '['
    spaces
    q <- nakedQuotations
    spaces
    char ']'
    spaces
    return (Quot q)

definitionHeader :: Parser String
definitionHeader = do
    name <- word
    string "=="
    spaces
    case name of
        Symbol x -> return x
        _        -> return "INVALID_DEFINITION_NAME"

definition :: Parser (String, WordP)
definition = do
    spaces
    string "DEFINE"
    spaces
    name <- definitionHeader
    q <- nakedQuotations
    spaces
    char ';'
    spaces
    return (name, Quotation q)

program :: Parser ([(String, WordP)], Stack)
program = do
    spaces
    ds <- many definition
    qs <- nakedQuotations
    return (ds, qs)
