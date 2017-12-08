module Primitives where

import           Data.Map    as M
import           Interpreter
-- import           Debug.Trace

--
-- Primitives
--
pop :: Lang -> Lang
pop lang = case stack lang of
    (_:cs) -> lang { stack = cs }
    _      -> lang { errors = "pop: stack underflow" : errors lang }

dup :: Lang -> Lang
dup lang = case stack lang of
    (c:cs) -> lang { stack = c : c : cs }
    _      -> lang { errors = "dup: stack empty" : errors lang }

dip :: Lang -> Lang
dip lang = case stack lang of
    (Quot q:c:cs) -> returnedLang { stack = c : stack returnedLang }
        where returnedLang = runQuotation q (lang { stack = cs })
    _ -> lang { errors = "dip: value and quotation expected" : errors lang }

def :: Lang -> Lang
def lang = case stack lang of
    (Quot q:Str s:cs) -> lang { vocab = vocab', stack = cs }
        where vocab' = insert s (Quotation q) (vocab lang)
    _ -> lang { errors = msg : errors lang }
        where msg = "def: string followed by quotation expected"

xP :: Lang -> Lang
xP lang = case stack lang of
    (Quot q:cs) -> rLang { stack = stack rLang ++ [Quot q] ++ cs }
        where rLang = runQuotation q (lang { stack = [] })
    _ -> lang { errors = msg : errors lang }
        where msg = "x: quotation must be executable without a stack"

iP :: Lang -> Lang
iP lang = case stack lang of
    (Quot q:cs) -> runQuotation q (lang { stack = cs })
    _ -> lang { errors = "i: quotation must be executable" : errors lang }

cons :: Lang -> Lang
cons lang = case stack lang of
    (Quot q:c    :cs) -> lang { stack = Quot (c : q) : cs }
    (Str  q:Chr c:cs) -> lang { stack = Str (c : q) : cs }
    _                 -> lang { errors = msg : errors lang }
        where msg = "cons: value then quotation or char then string expected"

uncons :: Lang -> Lang
uncons lang = case stack lang of
    (Quot (i:is):cs) -> lang { stack = Quot is : i : cs }
    (Str  (i:is):cs) -> lang { stack = Str is : Chr i : cs }
    _                -> lang { errors = msg : errors lang }
        where msg = "uncons: non empty quotation or string expected"

concatP :: Lang -> Lang
concatP lang = case stack lang of
    (Quot s:Quot t:cs) -> lang { stack = Quot (t ++ s) : cs }
    (Str s:Str t:cs) -> lang { stack = Str (t ++ s) : cs }
    _ -> lang { errors = "concatP: two quotations expected" : errors lang }

printVal :: Lang -> Lang
printVal lang = case stack lang of
    (c:cs) -> lang { stack = cs, result = formatV c : result lang }
    _      -> lang

ifThenElse :: Lang -> Lang
ifThenElse lang = case stack lang of
    (Quot qelse:Quot qthen:Quot qif:cs) -> if isTrue res
        then runQuotation qthen (lang { stack = cs })
        else runQuotation qelse (lang { stack = cs })
        where (res:_) = stack $ runQuotation qif (lang { stack = cs })
    _ -> lang { errors = "ifte: three quotations expected" : errors lang }

arithMulDiv :: (Double -> Double -> Double) -> Lang -> Lang
arithMulDiv operator lang = case (operator, stack lang) of
    (op, NumP y:NumP c:cs) -> lang { stack = NumP (op c y) : cs }
    (_ , _               ) -> lang { errors = msg : errors lang }
        where msg = "arithMulDiv: two numbers expected"

plus :: Lang -> Lang
plus lang = case stack lang of
    (NumP y:NumP c:cs) -> lang { stack = NumP (c + y) : cs }
    (NumP y:Chr  c:cs) -> lang { stack = Chr chr : cs }
        where chr = toEnum (fromEnum c + round y) :: Char
    _ -> lang { errors = msg : errors lang }
        where msg = "plus: two numbers or a char then an integer expected"

minus :: Lang -> Lang
minus lang = case stack lang of
    (NumP y:NumP c:cs) -> lang { stack = NumP (c - y) : cs }
    (NumP y:Chr  c:cs) -> lang { stack = Chr chr : cs }
        where chr = toEnum (fromEnum c - round y) :: Char
    _ -> lang { errors = msg : errors lang }
        where msg = "minus: two numbers or a char then an integer expected"

comparison :: (ValueP -> ValueP -> Bool) -> Lang -> Lang
comparison operator lang = case (operator, stack lang) of
    (op, y:c:cs) -> lang { stack = toTruth (op c y) : cs }
    (_ , _     ) -> lang { errors = msg : errors lang }
        where msg = "comparison operation: two numbers expected"

logic :: (Bool -> Bool -> Bool) -> Lang -> Lang
logic operator lang = case (operator, stack lang) of
    (op, y:c:cs) -> lang { stack = toTruth (op (isTrue c) (isTrue y)) : cs }
    (_ , _     ) -> lang { errors = msg : errors lang }
        where msg = "logic operation: two values expected"

lnot :: Lang -> Lang
lnot lang = case stack lang of
    (Str s:cs) -> lang { stack = toTruth (s == "") : cs }
    (c    :cs) -> lang { stack = toTruth (not (isTrue c)) : cs }
    _          -> lang { errors = "null: value expected" : errors lang }

stackP :: Lang -> Lang
stackP lang = lang { stack = Quot cs : cs } where cs = stack lang

unstack :: Lang -> Lang
unstack lang = case stack lang of
    (Quot ys:_) -> lang { stack = ys }
    _           -> lang { errors = "unstack: quotation expected" : errors lang }

list :: Lang -> Lang
list lang = case stack lang of
    (Quot _:cs) -> lang { stack = toTruth True : cs }
    (_     :cs) -> lang { stack = toTruth False : cs }
    _           -> lang { errors = "list: stack empty" : errors lang }

linrec :: Lang -> Lang
linrec lang = case stack lang of
    (Quot r2:Quot r1:Quot t:Quot p:cs) -> if isTrue res
        then runQuotation t (lang { stack = cs })
        else do
            let rLang  = runQuotation r1 (lang { stack = cs })
                cs'    = stack rLang
                stack' = Quot r2 : Quot r1 : Quot t : Quot p : cs'
                rLang' = linrec (rLang { stack = stack' })
            runQuotation r2 rLang'
        where (res:_) = stack $ runQuotation p (lang { stack = cs })
    _ -> lang
        { errors = "linrec: argument on stack are incorrect" : errors lang
        }

truncMod :: (RealFrac a, RealFrac a1) => a1 -> a -> Double
truncMod c y = fromInteger (truncate c `mod` truncate y) :: Double

primitives :: [(String, WordP)]
primitives =
    [ ("pop"    , Function pop)
    , ("dup"    , Function dup)
    , ("cons"   , Function cons)
    , ("uncons" , Function uncons)
    , ("concat" , Function concatP)
    , ("+"      , Function plus)
    , ("-"      , Function minus)
    , ("*"      , Function $ arithMulDiv (*))
    , ("/"      , Function $ arithMulDiv (/))
    , ("%"      , Function $ arithMulDiv truncMod)
    , ("="      , Function $ comparison (==))
    , ("<="     , Function $ comparison (<=))
    , (">="     , Function $ comparison (>=))
    , ("<"      , Function $ comparison (<))
    , (">"      , Function $ comparison (>))
    , ("and"    , Function $ logic (&&))
    , ("or"     , Function $ logic (||))
    , ("null"   , Function lnot)
    , ("stack"  , Function stackP)
    , ("unstack", Function unstack)
    , ("."      , Function printVal)
    , ("dip"    , Function dip)
    , ("x"      , Function xP)
    , ("i"      , Function iP)
    , ("ifte"   , Function ifThenElse)
    , ("list"   , Function list)
    , ("linrec" , Function linrec)
    , ("def"    , Function def)
    ]












