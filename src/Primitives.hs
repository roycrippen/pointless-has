module Primitives where

import           Interpreter (Lang (..), Value (..), WordP (..), formatV,
                              isTrue, runQuotation, toTruth)

-- import           Debug.Trace

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

arith :: (Double -> Double -> Double)->Lang -> Lang
arith operator lang = case (operator, stack lang) of
    (op, (Number y:(Number c:cs)))  -> lang { stack = Number (op c y) : cs }
    (_, _)                          -> lang { errors = "arithmetic operation: two numbers expected" : errors lang }

comparison :: (Double -> Double -> Bool)-> Lang -> Lang
comparison operator lang = case (operator, stack lang) of
    (op, (Number y:(Number c:cs)))  -> lang { stack = toTruth (op c y) : cs }
    (_, _)                          -> lang { errors = "comparison operation: two numbers expected" : errors lang }

logic :: (Bool -> Bool -> Bool)->Lang -> Lang
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

-- pop :: Stack -> Stack
-- pop (_:cs) = cs
-- pop _      = error "pop: stack underflow"

-- dup :: Stack -> Stack
-- dup (c:cs) = c : c : cs
-- dup _      = error "dup: stack empty"

-- dip :: Vocabulary -> Stack -> Stack
-- dip vocab (Quot q:(c:cs)) = c : runQuotation q vocab cs
-- dip _     _               = error "dip: value and quotation expected"

-- x :: Vocabulary -> Stack -> Stack
-- x vocab (Quot q:cs) = runQuotation q vocab [] ++ [Quot q] ++ cs
-- x _     _           = error "x: quotation must be executable without a stack"

-- i :: Vocabulary -> Stack -> Stack
-- i vocab (Quot q:cs) = runQuotation q vocab cs
-- i _     _           = error "x: quotation must be executable without a stack"

-- cons :: Stack -> Stack
-- cons (Quot q:(c:cs)) = Quot (c : q) : cs
-- cons _               = error "cons: value and quotation expected"

-- uncons :: Stack -> Stack
-- uncons (Quot (c:is):cs) = Quot is : c : cs
-- uncons _                = error "uncons: quotation with at least one element expected"

-- concatP :: Stack -> Stack
-- concatP (Quot s:(Quot t:cs)) = Quot (t ++ s) : cs
-- concatP _                    = error "concatP: two quotations expected"

-- printVal :: Stack -> Stack
-- printVal (_:cs) = cs
-- printVal _      = error "printVal: stack empty"

-- ifThenElse :: Vocabulary -> Stack -> Stack
-- ifThenElse vocab (Quot qelse:(Quot qthen:(Quot qif:cs))) = if isTrue result
--     then runQuotation qthen vocab cs
--     else runQuotation qelse vocab cs
--     where (result:_) = runQuotation qif vocab cs
-- ifThenElse _ _ = error "ifte: three quotations expected"

-- arith :: (Double -> Double -> Double) -> Stack -> Stack
-- arith op (Number y:(Number c:cs)) = Number (op c y) : cs
-- arith _ _                         = error "arithmetic operation: two numbers expected"

-- comparison :: (Double -> Double -> Bool) -> Stack -> Stack
-- comparison op (Number y:(Number c:cs)) = toTruth (op c y) : cs
-- comparison _ _                         = error "comparison operation: two numbers expected"

-- logic :: (Bool -> Bool -> Bool) -> Stack -> Stack
-- logic op (y:c:cs) = toTruth (op (isTrue c) (isTrue y)) : cs
-- logic _  _        = error "logic operation: two values expected"

-- lnot :: Stack -> Stack
-- lnot (c:cs) = toTruth (not (isTrue c)) : cs
-- lnot _      = error "null: value expected"

-- stackP :: Stack -> Stack
-- stackP cs = Quot cs : cs

-- unstack :: Stack -> Stack
-- unstack (Quot ys:_) = ys
-- unstack _           = error "unstack: quotation expected"

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


