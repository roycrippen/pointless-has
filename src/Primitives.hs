module Primitives where

import           Interpreter (Lang (..), Value (..), WordP (..), formatV,
                              isTrue, runQuotation, toTruth)

-- Primitives
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
    (Quot q:(c:cs)) -> returnedLang { stack = c : (stack returnedLang) }
        where returnedLang = runQuotation q (lang { stack = cs })
    _               -> lang { errors = "dip: value and quotation expected" : errors lang }

x :: Lang -> Lang
x lang = case stack lang of
    (Quot q:cs) -> returnedLang { stack = (stack returnedLang) ++ [Quot q] ++ cs }
        where returnedLang = runQuotation q (lang { stack = [] })
    _           -> lang { errors = "x: quotation must be executable without a stack" : errors lang }

i :: Lang -> Lang
i lang = case stack lang of
    (Quot q:cs) -> runQuotation q (lang { stack = cs })
    _           -> lang { errors = "i: quotation must be executable" : errors lang }

cons :: Lang -> Lang
cons lang = case stack lang of
    (Quot q:(c:cs)) -> lang { stack = Quot (c : q) : cs }
    _               -> lang { errors = "cons: value and quotation expected" : errors lang }

uncons :: Lang -> Lang
uncons lang = case stack lang of
    (Quot (c:is):cs) -> lang { stack = Quot is : c : cs }
    _                -> lang { errors = "uncons: quotation with at least one element expected" : errors lang }

concatP :: Lang -> Lang
concatP lang = case stack lang of
    (Quot s:(Quot t:cs)) -> lang { stack = Quot (t ++ s) : cs }
    _                -> lang { errors = "concatP: two quotations expected" : errors lang }

printVal :: Lang -> Lang
printVal lang = case stack lang of
    (c:cs) -> lang { stack = cs, display = formatV c : display lang }
    _      -> lang { errors = "printVal: stack empty" : errors lang }

ifThenElse :: Lang -> Lang
ifThenElse lang = case stack lang of
    (Quot qelse:(Quot qthen:(Quot qif:cs))) -> if isTrue result
        then runQuotation qthen (lang { stack = cs })
        else runQuotation qelse (lang { stack = cs })
      where (result:_) = stack $ runQuotation qif (lang { stack = cs })
    _ -> lang { errors = "ifte: three quotations expected" : errors lang }

arith :: (Double -> Double -> Double) -> Lang -> Lang
arith operator lang = case (operator, stack lang) of
    (op, (Number y:(Number c:cs)))  -> lang { stack = Number (op c y) : cs }
    (_, _)                          -> lang { errors = "arithmetic operation: two numbers expected" : errors lang }

comparison :: (Double -> Double -> Bool) -> Lang -> Lang
comparison operator lang = case (operator, stack lang) of
    (op, (Number y:(Number c:cs)))  -> lang { stack = toTruth (op c y) : cs }
    (_, _)                          -> lang { errors = "comparison operation: two numbers expected" : errors lang }

logic :: (Bool -> Bool -> Bool) -> Lang -> Lang
logic operator lang = case (operator, stack lang) of
    (op, (y:c:cs))  -> lang { stack = toTruth (op (isTrue c) (isTrue y)) : cs }
    (_, _)          -> lang { errors = "logic operation: two values expected" : errors lang }

lnot :: Lang -> Lang
lnot lang = case stack lang of
    (c:cs) -> lang { stack = toTruth (not (isTrue c)) : cs }
    _      -> lang { errors = "null: value expected" : errors lang }

stackP :: Lang -> Lang
stackP lang = lang { stack =  Quot cs : cs }
    where cs = stack lang

unstack :: Lang -> Lang
unstack lang = case stack lang of
    (Quot ys:_)-> lang { stack = ys }
    _      -> lang { errors = "unstack: quotation expected" : errors lang }

truncMod :: (RealFrac a, RealFrac a1) => a1 -> a -> Double
truncMod c y = fromInteger (truncate c `mod` truncate y) :: Double

primitives :: [(String, WordP)]
primitives =
    [ ("pop"    , Function pop)
    , ("dup"    , Function dup)
    , ("cons"   , Function cons)
    , ("uncons" , Function uncons)
    , ("concat" , Function concatP)
    , ("+"      , Function $ arith (+))
    , ("-"      , Function $ arith (-))
    , ("*"      , Function $ arith (*))
    , ("/"      , Function $ arith (/))
    , ("%"      , Function $ arith truncMod)
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
    , ("x"      , Function x)
    , ("i"      , Function i)
    , ("ifte"   , Function ifThenElse)
    ]


