module PointlessParser where

import           Interpreter (Stack, Value (..), WordP (..))
import           Parser      (Parser, anyChar, char, firstLetter, lookAhead,
                              many, manyTill, newline, numberDouble, spaces,
                              string, wordLetter, (<|>))

numberP :: Parser Value
numberP = do
    spaces
    comment
    d <- numberDouble
    spaces
    comment
    return (Number d)

word :: Parser Value
word = do
    spaces
    comment
    c <- firstLetter
    cs <- many wordLetter
    spaces
    comment
    return (Symbol (c:cs))

instruction :: Parser Value
instruction = quotation <|> word <|> numberP

nakedQuotations :: Parser Stack
nakedQuotations = many instruction

quotation :: Parser Value
quotation = do
    spaces
    comment
    char '['
    spaces
    q <- nakedQuotations
    spaces
    char ']'
    spaces
    comment
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
    comment
    string "DEFINE"
    spaces
    name <- definitionHeader
    q <- nakedQuotations
    spaces
    char ';'
    spaces
    comment
    return (name, Quotation q)

program :: Parser ([(String, WordP)], Stack)
program = do
    spaces
    ds <- many definition
    qs <- nakedQuotations
    return (ds, qs)

-- comment :: Parser ()
-- comment =
--     (char '#' >> manyTill anyChar newline >> spaces >> return ()) <|>
--     (string "(*" >> manyTill anyChar (string "*)") >> return () >> spaces >> return ())


comment :: Parser String
comment = commentMulti >> commentHash

commentMulti :: Parser String
commentMulti = do
    b <- lookAhead $ string "(*"
    if b
        then do
            string "(*"
            manyTill anyChar (string "*)")
            string "*)"
            spaces
            return "multi comment, ignored"
        else return "!multi comment, ignored"

commentHash :: Parser String
commentHash = do
    b <- lookAhead $ char '#'
    if b
        then do
            char '#'
            manyTill anyChar newline
            newline
            spaces
            return "hash comment, ignored"
        else return "!hash comment, ignored"

