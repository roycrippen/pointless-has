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
    _   <- spaces
    res <- numberP <|> charP <|> quotedStringP <|> quotation <|> word
    _   <- spaces
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
    _    <- spacesAndComments
    _    <- string "DEFINE"
    _    <- spaces
    name <- definitionHeader
    q    <- nakedQuotations
    _    <- spaces
    _    <- char ';'
    _    <- spacesAndComments
    return (name, Quotation q)

program :: Parser ([(String, WordP)], [ValueP])
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



