module Primitives where

import           Data.Map         as M
import           Data.Maybe       (fromJust, isJust)
import           Interpreter
import           System.IO.Unsafe (unsafePerformIO)
-- import           Debug.Trace

--
-- Primitives
--
pop :: Lang -> Lang
pop lang = case stack lang of
    (_:cs) -> lang { stack = cs }
    _      -> lang { result = "ERROR(pop): stack underflow" : result lang }

dup :: Lang -> Lang
dup lang = case stack lang of
    (c:cs) -> lang { stack = c : c : cs }
    _      -> lang { result = "ERROR(dup): stack empty" : result lang }

dip :: Lang -> Lang
dip lang = case stack lang of
    (Quot q:c:cs) -> returnedLang { stack = c : stack returnedLang }
        where returnedLang = runQuotation q (lang { stack = cs })
    _ -> lang { result = "ERROR(dip): value and quotation expected" : result lang }

define :: Lang -> Lang
define lang = case stack lang of
    (Quot q:Str s:cs) -> lang { vocab = vocab', stack = cs }
        where vocab' = insert s (Quotation q) (vocab lang)
    _ -> lang { result = msg : result lang }
        where msg = "ERROR(def): string followed by quotation expected"

xP :: Lang -> Lang
xP lang = case stack lang of
    (Quot q:cs) -> rLang { stack = stack rLang ++ [Quot q] ++ cs }
        where rLang = runQuotation q (lang { stack = [] })
    _ -> lang { result = msg : result lang }
        where msg = "ERROR(x): quotation must be executable without a stack"

iP :: Lang -> Lang
iP lang = case stack lang of
    (Quot q:cs) -> runQuotation q (lang { stack = cs })
    _           -> lang { result = "ERROR(i): quotation must be executable" : result lang }

cons :: Lang -> Lang
cons lang = case stack lang of
    (Quot q:c    :cs) -> lang { stack = Quot (c : q) : cs }
    (Str  q:Chr c:cs) -> lang { stack = Str (c : q) : cs }
    _                 -> lang { result = msg : result lang }
        where msg = "ERROR(cons): value then quotation or char then string expected"

uncons :: Lang -> Lang
uncons lang = case stack lang of
    (Quot (i:is):cs) -> lang { stack = Quot is : i : cs }
    (Str  (i:is):cs) -> lang { stack = Str is : Chr i : cs }
    _                -> lang { result = msg : result lang }
        where msg = "ERROR(uncons): non empty quotation or string expected"

concatP :: Lang -> Lang
concatP lang = case stack lang of
    (Quot s:Quot t:cs) -> lang { stack = Quot (t ++ s) : cs }
    (Str s:Str t:cs)   -> lang { stack = Str (t ++ s) : cs }
    _                  -> lang { result = "ERROR(concatP): two quotations expected" : result lang }

-- printVal :: Lang -> Lang
-- printVal lang = case stack lang of
--     (c:cs) -> lang { stack = cs, result = result', display = "" }
--       where result' = result lang ++ lines (display lang ++ formatV c)
--     []     -> lang { result = result', display = "" }
--       where result' = if display lang == ""
--                         then result lang
--                         else result lang ++ lines (display lang)
--
-- printVal :: Lang -> Lang
-- printVal lang@(Lang{ stack = c:cs}) = lang { stack = cs, result = result', display = "" }
--       where result' = result lang ++ lines (display lang ++ formatV c)

-- printVal lang@(Lang{ stack = []}) = lang { result = result', display = "" }
--       where result' = if display lang == ""
--                         then result lang
--                         else result lang ++ lines (display lang)
-- --

printVal :: Lang -> Lang
printVal lang@(Lang{ stack = c:cs, mode = m }) = if m == REPL
  then txRepl lang
  else txRepl lang'
    where
      result' = result lang ++ lines (display lang ++ formatV c)
      lang'   = lang { stack = cs, result = result', display = "" }

printVal lang@(Lang{ stack = [], mode = m }) = if m == REPL
  then txRepl lang
  else txRepl lang'
    where
      result' = if display lang == ""
                  then result lang
                  else result lang ++ lines (display lang)
      lang'  = lang { result = result', display = "" }

-- | immediately transmit output to console
txRepl :: Lang -> Lang
txRepl lang = unsafePerformIO  $ do
  mapM_ putStrLn (result lang)
  return lang { result = [] }

-- | immediately transmit output to console
-- txRepl :: Lang -> IO ()
-- txRepl lang = mapM_ putStrLn (result lang)

put :: Lang -> Lang
put lang = case stack lang of
    (c:cs) -> lang { stack = cs, display = display lang ++ formatV c }
    _      -> lang

putch :: Lang -> Lang
putch lang = case stack lang of
    (c:cs) -> lang { stack = cs, display = display', result = result' }
      where
        displayChar = formatPutch c
        display'    = if isJust displayChar
                        then display lang ++ [fromJust displayChar]
                        else display lang
        result'     = if isJust displayChar
                        then result lang
                        else "ERROR(putch): character or integer expected" : result lang
    _      -> lang

ifThenElse :: Lang -> Lang
ifThenElse lang = case stack lang of
    (Quot qelse:Quot qthen:Quot qif:cs) -> if isTrue res
        then runQuotation qthen (lang { stack = cs })
        else runQuotation qelse (lang { stack = cs })
        where (res:_) = stack $ runQuotation qif (lang { stack = cs })
    _ -> lang { result = "ERROR(ifte): three quotations expected" : result lang }

arithMulDiv :: (Double -> Double -> Double) -> Lang -> Lang
arithMulDiv operator lang = case (operator, stack lang) of
    (op, NumP y:NumP c:cs) -> lang { stack = NumP (op c y) : cs }
    (_ , _               ) -> lang { result = msg : result lang }
        where msg = "ERROR(arithMulDiv): two numbers expected"

plus :: Lang -> Lang
plus lang = case stack lang of
    (NumP y:NumP c:cs) -> lang { stack = NumP (c + y) : cs }
    (NumP y:Chr  c:cs) -> lang { stack = Chr chr : cs }
        where chr = toEnum (fromEnum c + round y) :: Char
    _ -> lang { result = msg : result lang }
        where msg = "ERROR(plus): two numbers or a char then an integer expected"

minus :: Lang -> Lang
minus lang = case stack lang of
    (NumP y:NumP c:cs) -> lang { stack = NumP (c - y) : cs }
    (NumP y:Chr  c:cs) -> lang { stack = Chr chr : cs }
        where chr = toEnum (fromEnum c - round y) :: Char
    _ -> lang { result = msg : result lang }
        where msg = "ERROR(minus): two numbers or a char then an integer expected"

comparison :: (ValueP -> ValueP -> Bool) -> Lang -> Lang
comparison operator lang = case (operator, stack lang) of
    (op, y:c:cs) -> lang { stack = toTruth (op c y) : cs }
    (_ , _     ) -> lang { result = msg : result lang }
        where msg = "ERROR(comparison operation): two numbers expected"

logic :: (Bool -> Bool -> Bool) -> Lang -> Lang
logic operator lang = case (operator, stack lang) of
    (op, y:c:cs) -> lang { stack = toTruth (op (isTrue c) (isTrue y)) : cs }
    (_ , _     ) -> lang { result = msg : result lang }
        where msg = "ERROR(logic operation): two values expected"

lnot :: Lang -> Lang
lnot lang = case stack lang of
    (Str s:cs) -> lang { stack = toTruth (s == "") : cs }
    (c    :cs) -> lang { stack = toTruth (not (isTrue c)) : cs }
    _          -> lang { result = "ERROR(null): value expected" : result lang }

stackP :: Lang -> Lang
stackP lang = lang { stack = Quot cs : cs } where cs = stack lang

unstack :: Lang -> Lang
unstack lang = case stack lang of
    (Quot ys:_) -> lang { stack = ys }
    _           -> lang { result = "ERROR(unstack): quotation expected" : result lang }

list :: Lang -> Lang
list lang = case stack lang of
    (Quot _:cs) -> lang { stack = toTruth True : cs }
    (_     :cs) -> lang { stack = toTruth False : cs }
    _           -> lang { result = "ERROR(list): stack empty" : result lang }

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
        { result = "ERROR(linrec): argument on stack are incorrect" : result lang
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
    , ("put"    , Function put)
    , ("putch"  , Function putch)
    , ("dip"    , Function dip)
    , ("x"      , Function xP)
    , ("i"      , Function iP)
    , ("ifte"   , Function ifThenElse)
    , ("list"   , Function list)
    , ("linrec" , Function linrec)
    , ("define" , Function define)
    ]

