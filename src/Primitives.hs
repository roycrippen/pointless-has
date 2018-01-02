module Primitives where

import           Data.Aeson.Text  (encodeToLazyText)
import           Data.Map         as M (insert)
import           Data.Maybe       (fromJust, isJust)
import           Data.Text        (Text)
import qualified Data.Text        as T (pack)
import qualified Data.Text.Lazy   as TL (toStrict)
import           Interpreter
import           Parser           (parse)
import           PointlessParser  (nakedQuotations)
import           System.IO.Error  (tryIOError)
import           System.IO.Unsafe (unsafePerformIO)
-- import qualified Network.WebSockets as WS (sendTextData)
-- import           Debug.Trace

-- |
-- | Implementation of primitive functions
-- |
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
        where vocab' = M.insert s (Quotation q) (vocab lang)
    _ -> lang { result = msg : result lang }
        where msg = "ERROR(def): string followed by quotation expected"

eval :: Lang -> Lang
eval lang = case stack lang of
    (Quot q:cs) -> rLang { stack = stack rLang ++ [Quot q] ++ cs }
        where rLang = runQuotation q (lang { stack = [] })
    _ -> lang { result = msg : result lang }
        where msg = "ERROR(x): quotation must be executable without a stack"

exec :: Lang -> Lang
exec lang = case stack lang of
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

tx :: Lang -> Lang
tx lang = case stack lang of
  (c : cs) -> txMode (lang { stack = cs, result = result', display = "" })
    where result' = result lang ++ lines (display lang ++ formatV c)
  []       ->  txMode (lang { result = result', display = "" })
    where result' = if display lang == ""
                      then result lang
                      else result lang ++ lines (display lang)

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

sqrtP :: Lang -> Lang
sqrtP lang = case stack lang of
    (NumP y:cs) -> lang { stack = NumP (sqrt y) : cs }
    _ -> lang { result = msg : result lang }
        where msg = "ERROR(sqrt): a number expected"

sinP :: Lang -> Lang
sinP lang = case stack lang of
    (NumP y:cs) -> lang { stack = NumP (sin y) : cs }
    _ -> lang { result = msg : result lang }
        where msg = "ERROR(sqrt): a number expected"


cosP :: Lang -> Lang
cosP lang = case stack lang of
    (NumP y:cs) -> lang { stack = NumP (cos y) : cs }
    _ -> lang { result = msg : result lang }
        where msg = "ERROR(sqrt): a number expected"

tanP :: Lang -> Lang
tanP lang = case stack lang of
    (NumP y:cs) -> lang { stack = NumP (tan y) : cs }
    _ -> lang { result = msg : result lang }
        where msg = "ERROR(sqrt): a number expected"

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

isList :: Lang -> Lang
isList lang = case stack lang of
    (Quot _:cs) -> lang { stack = toTruth True : cs }
    (_     :cs) -> lang { stack = toTruth False : cs }
    _           -> lang { result = "ERROR(list?): stack empty" : result lang }

isString :: Lang -> Lang
isString lang = case stack lang of
    (Str _:cs)  -> lang { stack = toTruth True : cs }
    (_     :cs) -> lang { stack = toTruth False : cs }
    _           -> lang { result = "ERROR(string?): stack empty" : result lang }

isNumber :: Lang -> Lang
isNumber lang = case stack lang of
    (NumP _:cs) -> lang { stack = toTruth True : cs }
    (_     :cs) -> lang { stack = toTruth False : cs }
    _           -> lang { result = "ERROR(number?): stack empty" : result lang }

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

libopen :: Lang -> Lang
libopen lang = case stack lang of
  (Str  s:cs) -> rxFile s (lang { stack = cs })
  _           -> lang { result = msg : result lang }
    where msg = "ERROR(libopen): string file name expected"

showP :: Lang -> Lang
showP lang = case stack lang of
    (c:cs) -> lang { stack = s : cs }
      where s = Str (formatV c)
    _      -> lang { result = "ERROR(dup): stack empty" : result lang }

truncMod :: (RealFrac a, RealFrac a1) => a1 -> a -> Double
truncMod c y = fromInteger (truncate c `mod` truncate y) :: Double

-- | Serializes a Lang to JSON.
jsonResultsShow :: Lang -> Text
jsonResultsShow lang = T.pack "{\n" `mappend` text `mappend` T.pack "\n}"
  where
    stackT   = encodeP "\"stack\":" (formatStack $ stack lang)
    resultT  = encodeP "\"result\":" (result lang)
    displayT = encodeP "\"display\":" [display lang]
    newline  = T.pack ",\n"
    text     = stackT `mappend` newline
                      `mappend` resultT
                      `mappend` newline
                      `mappend` displayT

encodeP :: String -> [String] -> Text
encodeP s xs = T.pack s `mappend` TL.toStrict (encodeToLazyText xs)


-- | run a quotation string
runQuotStr :: String -> Lang -> Lang
runQuotStr s = runQuotation qs
  where (qs, _):_ = parse nakedQuotations s


-- | limited unsafe IO actions
-- |
-- | transmit output (UNSAFE)
txMode :: Lang -> Lang
txMode lang@Lang{mode = m} = unsafePerformIO  $
  case m of
    REPL -> do
      mapM_ putStrLn (result lang)
      return lang { result = [] }
    WEBSOCKET _ -> return lang
      -- does not work for WS, kills conn
      -- WEBSOCKET conn -> do
      -- let resultsJSON = jsonResultsShow lang
      -- mapM_ putStrLn (result lang)
      -- WS.sendTextData conn (resultsJSON :: Text)
      -- return lang { result = [] }

-- | read source from file (UNSAFE)
rxFile :: String -> Lang -> Lang
rxFile file lang = unsafePerformIO $ do
  strOrExc <- tryIOError $ readFile file
  case strOrExc of
    Left except -> do
      print except
      return lang
    Right source' -> do
      let source = replaceStr  "\\n" "\n" (removeDocLines source')
          lang' = runQuotStr source lang
      -- mapM_ putStrLn (result lang')
      return lang'

removeDocLines :: String -> String
removeDocLines str = unlines xs
      where xs = filter (\x -> (take 8 x) == "        ") $ lines str
