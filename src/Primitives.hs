module Primitives where

import           Interpreter (Stack, Value (..), Vocabulary, WordP (..), isTrue,
                              runQuotation, toTruth)

-- import           Debug.Trace

-- Primitives
pop :: Stack -> Stack
pop (_:cs) = cs
pop _      = error "pop: stack underflow"

dup :: Stack -> Stack
dup (c:cs) = c:c:cs
dup _      = error "dup: stack empty"

dip :: Vocabulary -> Stack -> Stack
dip vocab (Quot q : (c : cs)) = c:(runQuotation q vocab cs)
dip _ _                       = error "dip: value and quotation expected"

x :: Vocabulary -> Stack -> Stack
x vocab (Quot q : cs) = (runQuotation q vocab []) ++ [Quot q] ++ cs
x _ _                 = error "x: quotation must be executable without a stack"

i :: Vocabulary -> Stack -> Stack
i vocab (Quot q : cs) = runQuotation q vocab cs
i _ _                 = error "x: quotation must be executable without a stack"

cons :: Stack -> Stack
cons (Quot q : (c : cs)) = Quot (c:q) : cs
cons _                   = error "cons: value and quotation expected"

uncons :: Stack -> Stack
uncons (Quot (c : is) : cs) = Quot is : c : cs
uncons _ = error "uncons: quotation with at least one element expected"

concatP :: Stack -> Stack
concatP (Quot s : (Quot t : cs)) = Quot (t ++ s) : cs
concatP _                        = error "concatP: two quotations expected"

printVal :: Stack -> Stack
printVal (_:cs) = cs
printVal _      = error "printVal: stack empty"

ifThenElse :: Vocabulary -> Stack -> Stack
ifThenElse vocab (Quot qelse : (Quot qthen : (Quot qif : cs))) = do
    if isTrue result
        then runQuotation qthen vocab cs
        else runQuotation qelse vocab cs
      where
        (result:_) = runQuotation qif vocab cs

ifThenElse _ _ = error "ifte: three quotations expected"

arith :: (Double -> Double -> Double) -> Stack -> Stack
arith op (Number y : (Number c : cs)) = Number(op c y):cs
arith _ _ = error "arithmetic operation: two numbers expected"

comparison :: (Double -> Double -> Bool) -> Stack -> Stack
comparison op (Number y : (Number c : cs)) = toTruth(op c y):cs
comparison _ _ = error "comparison operation: two numbers expected"

logic :: (Bool -> Bool -> Bool) -> Stack -> Stack
logic op (y:c:cs) = toTruth (op (isTrue c) (isTrue y)) : cs
logic _ _         = error "logic operation: two values expected"

lnot :: Stack -> Stack
lnot (c:cs) = toTruth (not (isTrue c)) : cs
lnot _      = error "null: value expected"

stack :: Stack -> Stack
stack cs = Quot cs : cs

unstack :: Stack -> Stack
unstack (Quot ys : _) = ys
unstack _             = error "unstack: quotation expected"

truncMod :: (RealFrac a, RealFrac a1) => a1 -> a -> Double
truncMod c y = fromInteger (truncate c `mod` truncate y) :: Double

primitives :: [(String, WordP)]
primitives =
    [ ("pop",      Primitive pop)
    , ("dup",      Primitive dup)
    , ("cons",     Primitive cons)
    , ("uncons",   Primitive uncons)
    , ("concat",   Primitive concatP)
    , ("+",        Primitive $ arith (+))
    , ("-",        Primitive $ arith (-))
    , ("*",        Primitive $ arith (*))
    , ("/",        Primitive $ arith (/))
    , ("%",        Primitive $ arith truncMod)
    , ("=",        Primitive $ comparison (==))
    , ("<=",       Primitive $ comparison (<=))
    , (">=",       Primitive $ comparison (>=))
    , ("<",        Primitive $ comparison (<))
    , (">",        Primitive $ comparison (>))
    , ("and",      Primitive $ logic (&&))
    , ("or",       Primitive $ logic (||))
    , ("null",     Primitive lnot)
    , ("stack",    Primitive stack)
    , ("unstack",  Primitive unstack)
    , (".",        Primitive printVal)
    , ("dip",      Function dip)
    , ("x",        Function x)
    , ("i",        Function i)
    , ("ifte",     Function ifThenElse)
    ]


--     primitives :: [(String, WordP)]
-- primitives =
--     [ ("pop",      Primitive pop)
--     , ("dup",      Primitive dup)
--     , ("dip",      EnvPrimitive dip)
--     , ("x",        EnvPrimitive x)
--     , ("i",        EnvPrimitive i)
--     , ("cons",     Primitive cons)
--     , ("uncons",   Primitive uncons)
--     , ("concat",   Primitive concatP)
--     , (".",        EnvPrimitive printVal)
--     , ("ifte",     EnvPrimitive ifThenElse)
--     , ("+",        Primitive $ arith (+))
--     , ("-",        Primitive $ arith (-))
--     , ("*",        Primitive $ arith (*))
--     , ("/",        Primitive $ arith (/))
--     , ("%",        Primitive $ arith truncMod)
--     , ("=",        Primitive $ comparison (==))
--     , ("<=",       Primitive $ comparison (<=))
--     , (">=",       Primitive $ comparison (>=))
--     , ("<",        Primitive $ comparison (<))
--     , (">",        Primitive $ comparison (>))
--     , ("and",      Primitive $ logic (&&))
--     , ("or",       Primitive $ logic (||))
--     , ("null",     Primitive lnot)
--     , ("stack",    Primitive stack)
--     , ("unstack",  Primitive unstack)
--     ]
