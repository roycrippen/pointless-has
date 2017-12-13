module PointlessParser where

-- import           Debug.Trace
import           Interpreter
import           Parser      (Parser, anyChar, char, firstLetter, many, manyTill, newline,
                              numberDouble, quotedString, spaces, string, wordLetter, (<|>))

numberP :: Parser ValueP
numberP = do
    d <- numberDouble
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
    return (Str str)

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
    _    <- spacesCommentsSpecifications
    _    <- string "DEFINE"
    _    <- spaces
    name <- definitionHeader
    q    <- nakedQuotations
    _    <- spaces
    _    <- char ';'
    _    <- spacesCommentsSpecifications
    return (name, Quotation q)

program :: Parser ([(String, WordP)], [ValueP])
program = do
    _  <- spacesCommentsSpecifications
    ds <- many definition
    _  <- spaces
    _  <- comments
    qs <- nakedQuotations
    _  <- spacesCommentsSpecifications
    return (ds, qs)

comment :: Parser ()
comment =
    (string "$" >> manyTill anyChar newline >> spaces >> return ())
        <|> (  char '{'
            >> manyTill anyChar (char '}')
            >> char '}'
            >> spaces
            >> return ()
            )

comments :: Parser [()]
comments = many comment

specification :: Parser ()
specification =
            char '('
            >> manyTill anyChar (char ')')
            >> char ')'
            >> spaces
            >> return ()

specifications :: Parser [()]
specifications = many specification

spacesCommentsSpecifications :: Parser ()
spacesCommentsSpecifications = spaces >> comments >> specifications >> return ()

