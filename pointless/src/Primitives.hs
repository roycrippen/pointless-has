module Primitives where

import           Clash.Prelude    hiding (length, many, (++), (<|>))
import           Data.Aeson.Text  (encodeToLazyText)
import           Data.Bool
import           Data.Char
import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.List        as L
import           Data.Map         as M (fromList, insert)
import           Data.Maybe       (fromJust, isJust)
import           Data.String
import           Data.Text        (Text)
import qualified Data.Text        as T (pack)
import qualified Data.Text.Lazy   as TL (toStrict)
import           Interpreter
import           Parser           (nakedQuotations, parse, tests)
import           Prelude          as P
import           System.IO.Error  (tryIOError)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Read
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
  _ ->
    lang { result = "ERROR(dip): value and quotation expected" : result lang }

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
   where
    msg = "ERROR(cons): value then quotation or char then string expected"

unconsP :: Lang -> Lang
unconsP lang = case stack lang of
  (Quot (i:is):cs) -> lang { stack = Quot is : i : cs }
  (Str  (i:is):cs) -> lang { stack = Str is : Chr i : cs }
  _                -> lang { result = msg : result lang }
    where msg = "ERROR(uncons): non empty quotation or string expected"

concatP :: Lang -> Lang
concatP lang = case stack lang of
  (Quot s:Quot t:cs) -> lang { stack = Quot (t ++ s) : cs }
  (Str s:Str t:cs)   -> lang { stack = Str (t ++ s) : cs }
  _                  -> lang { result = "ERROR(concat): two quotations expected" : result lang }

size :: Lang -> Lang
size lang = case stack lang of
  (Quot a:cs) -> lang { stack = NumP (fromIntegral $ length a) : cs }
  (Str s:cs)  -> lang { stack = NumP (fromIntegral $ length s) : cs }
  _           -> lang { result = "ERROR(concat): two quotations expected" : result lang }

tx :: Lang -> Lang
tx lang = case stack lang of
  (c:cs) -> txMode (lang { stack = cs, result = result', display = "" })
    where result' = result lang ++ lines (display lang ++ formatV c)
  [] -> txMode (lang { result = result', display = "" })
   where
    result' = if display lang == ""
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
    result' = if isJust displayChar
      then result lang
      else "ERROR(putch): character or integer expected" : result lang
  _ -> lang

ifThenElse :: Lang -> Lang
ifThenElse lang = case stack lang of
  (Quot f:Quot t:Quot b:cs) -> if isTrue res
    then runQuotation t (lang { stack = cs })
    else runQuotation f (lang { stack = cs })
    where (res:_) = stack $ runQuotation b (lang { stack = cs })
  _ -> lang { result = "ERROR(ifte): three quotations expected" : result lang }

arithMulDiv :: (Int -> Int -> Int) -> Lang -> Lang
arithMulDiv operator lang = case (operator, stack lang) of
  (op, NumP y:NumP c:cs) -> lang { stack = NumP (op c y) : cs }
  (_ , _               ) -> lang { result = msg : result lang }
    where msg = "ERROR(arithMulDiv): two numbers expected"

plusP :: Lang -> Lang
plusP lang = case stack lang of
  (NumP y:NumP c:cs) -> lang { stack = NumP (c + y) : cs }
  (NumP y:Chr  c:cs) -> lang { stack = Chr chr : cs }
    where chr = toEnum (fromEnum c + y) :: Char
  _ -> lang { result = msg : result lang }
    where msg = "ERROR(plus): two numbers or a char then an integer expected"

-- sqrtP :: Lang -> Lang
-- sqrtP lang = case stack lang of
--   (NumP y:cs) -> lang { stack = NumP (sqrt y) : cs }
--   _           -> lang { result = msg : result lang }
--     where msg = "ERROR(sqrt): a number expected"

-- sinP :: Lang -> Lang
-- sinP lang = case stack lang of
--   (NumP y:cs) -> lang { stack = NumP (sin y) : cs }
--   _           -> lang { result = msg : result lang }
--     where msg = "ERROR(sin): a number expected"


-- cosP :: Lang -> Lang
-- cosP lang = case stack lang of
--   (NumP y:cs) -> lang { stack = NumP (cos y) : cs }
--   _           -> lang { result = msg : result lang }
--     where msg = "ERROR(cos): a number expected"

-- tanP :: Lang -> Lang
-- tanP lang = case stack lang of
--   (NumP y:cs) -> lang { stack = NumP (tan y) : cs }
--   _           -> lang { result = msg : result lang }
--     where msg = "ERROR(tan): a number expected"

minusP :: Lang -> Lang
minusP lang = case stack lang of
  (NumP y:NumP c:cs) -> lang { stack = NumP (c - y) : cs }
  (NumP y:Chr  c:cs) -> lang { stack = Chr chr : cs }
    where chr = toEnum (fromEnum c - y) :: Char
  _ -> lang { result = msg : result lang }
    where msg = "ERROR(minus): two numbers or a char then an integer expected"
--
modP :: Lang -> Lang
modP lang = case stack lang of
  (NumP y:NumP c:cs) -> lang { stack = NumP (c `mod` y) : cs }
  _                  -> lang { result = msg : result lang }
    where msg = "ERROR(mod): two numbers expected"

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
  (Str _:cs) -> lang { stack = toTruth True : cs }
  (_    :cs) -> lang { stack = toTruth False : cs }
  _          -> lang { result = "ERROR(string?): stack empty" : result lang }

isNumberP :: Lang -> Lang
isNumberP lang = case stack lang of
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

_libopen :: Lang -> Lang
_libopen lang = case stack lang of
  (Str s:cs) -> runQuotStr (rxFile s) (lang { stack = cs })
  _          -> lang { result = msg : result lang }
    where msg = "ERROR(libopen): string file name expected"

_runtests :: Lang -> Lang
_runtests lang = case stack lang of
  [Str s] -> do
    let lang' = loadLibForTests lang
        qs    = testQuots $ rxFile s
    runQuotation qs lang' { stack = [] }
  _ -> lang { result = msg : result lang }
    where msg = "ERROR(_runtests): string file name expected"

showP :: Lang -> Lang
showP lang = case stack lang of
  (c:cs) -> lang { stack = s : cs } where s = Str (formatV c)
  _      -> lang { result = "ERROR(show): stack empty" : result lang }

  -- truncMod :: (RealFrac a, RealFrac a1) => a1 -> a -> Double
  -- truncMod c y = fromInteger (truncate c `mod` truncate y) :: Double

-- | Serializes a Lang to JSON.
jsonResultsShow :: Lang -> Text
jsonResultsShow lang = T.pack "{\n" `mappend` text `mappend` T.pack "\n}"
 where
  stackT   = encodeP "\"stack\":" (formatStack $ stack lang)
  resultT  = encodeP "\"result\":" (result lang)
  displayT = encodeP "\"display\":" [display lang]
  newline  = T.pack ",\n"
  text =
    stackT
      `mappend` newline
      `mappend` resultT
      `mappend` newline
      `mappend` displayT

encodeP :: String -> [String] -> Text
encodeP s xs = T.pack s `mappend` TL.toStrict (encodeToLazyText xs)

-- | run a quotation string
runQuotStr :: String -> Lang -> Lang
runQuotStr s = runQuotation qs where (qs, _):_ = parse nakedQuotations s

primitiveAST :: Vocabulary -> [ValueP] -> [ValueP]
primitiveAST vocab []           = []
primitiveAST vocab (Str  s :vs) = Str s : primitiveAST vocab vs
primitiveAST vocab (NumP n :vs) = NumP n : primitiveAST vocab vs
primitiveAST vocab (Chr  c :vs) = Chr c : primitiveAST vocab vs
primitiveAST vocab (Quot qs:vs) = case qs of
  [] -> Quot qs : primitiveAST vocab vs
  _  -> Quot (primitiveAST vocab qs) : primitiveAST vocab vs
primitiveAST vocab (Sym sym:vs) = case getWord sym vocab of
  Nothing   -> Sym sym : primitiveAST vocab vs
  Just word -> case word of
    Function  f  -> Sym sym : primitiveAST vocab vs
    Quotation qs -> primitiveAST vocab qs ++ primitiveAST vocab vs

-- | limited unsafe IO actions
-- |
-- | transmit output (UNSAFE)
txMode :: Lang -> Lang
txMode lang@Lang { mode = m } = unsafePerformIO $ case m of
  REPL -> do
    mapM_ putStrLn (result lang)
    return lang { result = [] }
  WEBSOCKET -> return lang

-- | read source from file (UNSAFE)
rxFile :: String -> String
rxFile file = unsafePerformIO $ do
  strOrExc <- tryIOError $ readFile file
  case strOrExc of
    Left except -> do
      print except
      return ""
    Right source -> do
      let source' = replaceStr "\\n" "\n" (removeDocLines source)
      return source'

-- | strip away col 1 to 8 comment lines
removeDocLines :: String -> String
removeDocLines str = unlines xs
  where xs = filter (\x -> L.take 8 x == "        ") $ lines str

-- | helper functions to get inline tests from inside {}
loadLibForTests :: Lang -> Lang
loadLibForTests lang = case stack lang of
  [Str s] -> do
    let qs     = inlineSource $ getQuotsFromFile s
        qs'    = keepDefines qs
        lang'  = runQuotation qs' lang { stack = [] }
        lang'' = lang' { result = result lang' ++ result lang }
    lang''
  _ -> lang { result = msg : result lang }
    where msg = "ERROR(runTests): string file name expected"

-- | load and parse source string from a file
getQuotsFromFile :: String -> [ValueP]
getQuotsFromFile s = qs where (qs, _):_ = parse nakedQuotations $ rxFile s

-- add argument to prevent circular references
-- this will crash if a libloads b and b libloads a
-- | replace libload commends with actual source from a file
inlineSource :: [ValueP] -> [ValueP]
inlineSource []                 = []
inlineSource (Str s:Sym sym:vs) = case sym of
  "libload" ->
    inlineSource (getQuotsFromFile (s ++ ".pless")) ++ inlineSource vs
  "libopen" -> inlineSource (getQuotsFromFile s) ++ inlineSource vs
  _         -> Str s : Sym sym : inlineSource vs
inlineSource (s:vs) = s : inlineSource vs

-- | throw away all commands that are not a kind of define, libload or libopen
keepDefines :: [ValueP] -> [ValueP]
keepDefines []                        = []
keepDefines (Str s:Quot q:Sym sym:vs) = if sym == "define"
  then Str s : Quot q : Sym sym : keepDefines vs
  else keepDefines vs
keepDefines (Quot q:Sym sym:vs) = if sym == "defines" || sym == "dictionary"
  then Quot q : Sym sym : keepDefines vs
  else keepDefines vs
keepDefines (Str s:Sym sym:vs) = if sym == "libload" || sym == "libopen"
  then Str s : Sym sym : keepDefines vs
  else keepDefines vs
keepDefines (_:vs) = keepDefines vs

-- | parse a source string returning just the test code inside of {}
testQuots :: String -> [ValueP]
testQuots s = do
  (ts, _) <- parse tests s
  if not (null ts)
    then do
      let (qs, _):_ = parse nakedQuotations $ unlines ts
      qs
    else []


coreDefinitions :: Vocabulary
coreDefinitions = M.fromList $ getQuotations coreLibrary ++ primitives

getQuotation :: (String, String) -> (String, WordP)
getQuotation (name, qs) = (name, Quotation q)
  where (q, _):_ = parse nakedQuotations qs

getQuotations :: [(String, String)] -> [(String, WordP)]
getQuotations = L.map getQuotation

primitives :: [(String, WordP)]
primitives =
  [ ("pop"      , Function pop)
  , ("dup"      , Function dup)
  , ("dip"      , Function dip)
  , ("cons"     , Function cons)
  , ("uncons"   , Function unconsP)
  , ("concat"   , Function concatP)
  , ("size"     , Function size)
  , ("+"        , Function plusP)
  , ("-"        , Function minusP)
  , ("*"        , Function $ arithMulDiv (*))
  , ("/"        , Function $ arithMulDiv div)
  , ("%"        , Function modP)
  , ("="        , Function $ comparison (==))
  , ("!="       , Function $ comparison (/=))
  , ("<="       , Function $ comparison (<=))
  , (">="       , Function $ comparison (>=))
  , ("<"        , Function $ comparison (<))
  , (">"        , Function $ comparison (>))
  , ("and"      , Function $ logic (&&))
  , ("or"       , Function $ logic (||))
  , ("null"     , Function lnot)
  , ("list?"    , Function isList)
  , ("string?"  , Function isString)
  , ("number?"  , Function isNumberP)
  , ("stack"    , Function stackP)
  , ("unstack"  , Function unstack)
  , ("show"     , Function showP)
  , ("eval"     , Function eval)
  , ("exec"     , Function exec)
  , ("ifte"     , Function ifThenElse)
  , ("linrec"   , Function linrec)
  , ("define"   , Function define)
  , ("tx"       , Function tx)
  , ("put"      , Function put)
  , ("putch"    , Function putch)
  , ("_libopen" , Function _libopen)
  , ("_runtests", Function _runtests)
  ]
    -- , ("binrec"   , Function binrec)
    -- , ("sqrt"     , Function sqrtP)
    -- , ("sin"      , Function sinP)
    -- , ("cos"      , Function cosP)
    -- , ("tan"      , Function tanP)



coreLibrary :: [(String, String)]
coreLibrary =
  [ ("true"    , "1")
  , ("false"   , "0")
  , ("rem"     , "%")
  , ("not"     , "null")
  , ("unitlist", "[] cons")
  , ("swap"    , "unitlist dip")
  , ("pop2"    , "pop pop")
  , ("pop3"    , "pop pop pop")
  , ("popd"    , "[pop] dip")
  , ("dupd"    , "[dup] dip")
  , ("swapd"   , "[swap] dip")
  , ("swons"   , "swap cons")
  , ("first"   , "uncons pop")
  , ("rest"    , "uncons swap pop")
  , ("over"    , "[dup] dip swap")
  , ("over2"   , "[over] dip swap")
  , ("over3"   , "[over2] dip swap")
  , ("dup2"    , "over over")
  , ("dup3"    , "over2 over2 over2")
  , ("dip1"    , "dip")
  , ("dip2"    , "[dip] cons dip")
  , ("dip3"    , "[dip] cons [dip] cons dip")
  , ("nullary" , "stack [exec] dip cons unstack swap pop")
  , ("unary"   , "stack [exec] dip cons unstack [pop2] dip")
  , ("binary"  , "stack [exec] dip cons unstack [pop3] dip")
  , ("exec2"   , "[dip] dip exec")
  , ("unary2"  , "[unary  ] cons dup exec2")
  , ("infra"   , "swons [stack] dip cons unstack [exec stack] dip cons unstack")
  , ("cleave"  , "[dup] dip2 swap dip2 exec")
  , ("branch"  , "[] rollup ifte")
  , ("in", "swap [=] cons filter size [1 >=] [true] [false] ifte swap pop")
  , ("times"   , "[ pop not ] [ pop2 ] [ [ pred ] dip dup dip2 ] tailrec")
  , ("repeat"  , "[swons] cons [] rollup times")
  , ( "step"
    , "[null2] [pop2] [[uncons] dip dup dip2] tailrec"
    )
  -- , ("size"    , "0 swap [pop succ] step")
  , ("map", "swap [[]] [\"\"] iflist swap rolldown [swons] concat step reverse")
  , ("fold", "swapd step")
  , ( "filter"
    , "swap [[]] [\"\"] iflist swap rolldown [[swons] [pop] ifte] cons step reverse"
    )
  , ("even"    , "2 % 0 =")
  , ("odd"     , "even not")
  , ("swoncat" , "swap concat")
  , ("when"    , "[] ifte")
  , ("unless"  , "[] swap ifte")
  , ("neg"     , "-1 *")
  , ("abs"     , "[0 <] [neg] when")
  , ("keep"    , "dupd dip")
  , ("rolldown", "swapd swap")
  , ("rollup"  , "[unitlist cons] dip swap exec")
  , ("iflist"  , "[list?] rollup ifte")
  , ("unswons" , "uncons swap")
  , ("shunt"   , "[swons] step")
  , ("reverse" , "[[]] [\"\"] iflist swap shunt")
  , ("reversed", "[reverse] dip")
  , ("last"    , "reverse first")
  , ("sum"     , "0 [+] fold")
  , ("product" , "1 [*] fold")
  , ("succ"    , "1 +")
  , ("pred"    , "1 -")
  , ("newstack", "[] unstack")
  , ("sequor"  , "[pop true] swap ifte")
  , ("sequand" , "[pop false] ifte")
  , ("dipd"    , "[dip] cons dip")
  , ("cleave"  , "[dup] dip2 swap dip2 exec")
  , ("true"    , "1")
  , ("false"   , "0")
  , ("truth"   , "true")
  , ("falsity" , "false")
  , ("conjoin" , "[[false] ifte] cons cons ")
  , ("disjoin" , "[ifte] cons [true] swons cons")
  , ("call"    , "unitlist exec")
  , ("drop"    , "[rest] times")
  , ("pairlist", "unitlist cons")
  , ("unpair"  , "uncons uncons pop")
  , ("second"  , "rest first")
  , ("third"   , "rest rest first")
  , ("fourth"  , "3 drop first")
  , ("fifth"   , "4 drop first")
  , ("nulld"   , "[null] dip")
  , ("consd"   , "[cons] dip")
  , ("swonsd"  , "[swons] dip")
  , ("unconsd" , "[uncons] dip")
  , ("unswonsd", "[unswons] dip")
  , ("firstd"  , "[first] dip")
  , ("restd"   , "[rest] dip")
  , ("secondd" , "[second] dip")
  , ("thirdd"  , "[third] dip")
  , ("null2"   , "nulld null or")
  , ("cons2"   , "swapd cons consd")
  , ("uncons2" , "unconsd uncons swapd")
  , ("swons2"  , "swapd swons swonsd")
  , ("unswons2", "[unswons] dip unswons swapd")
  , ("zip"     , "[null2] [pop2 []] [uncons2] [[pairlist] dip cons] linrec")
  , ( "from-to"
    , "unitlist [pop pop] swoncat [>] swap [[dup succ] dip] [cons] linrec"
    )
  , ("from-to-list"  , "[] from-to")
  , ("from-to-string", "\"\" from-to")
  , ("tailrec"       , "[] linrec")
  , ("split"         , "dup2 filter rollup [ not ] concat filter")
  , ("pairstep"      , "[dupd] swoncat [step pop] cons cons step")
  , ("mapr"          , "[[null] [] [uncons]] dip [dip cons] cons linrec")
  , ("foldr", "[[[null]] dip unitlist [pop] swoncat [uncons]] dip linrec")
  , ( "stepr2"
    , "[[null2] [pop pop]] dip [dip] cons [dip] cons [uncons2] swoncat tailrec"
    )
  , ("mapr2", "[[null2] [pop2 []] [uncons2]] dip [dip cons] cons linrec")
  , ( "foldr2"
    , "[[ [null2] ] dip unitlist [pop2] swoncat [uncons2] ] dip linrec"
    )
  , ("interleave2"    , "[cons cons] foldr2")
  , ("interleave2list", "[] interleave2 ")
  , ("average"        , "[ sum ] [ size ] cleave / ")
  , ("while"          , "swap [not] concat [] rolldown tailrec")
  , ("to-upper"       , "['a' >= ] [32 -] when")
  , ("to-lower"       , "['a' <  ] [32 +] when")
  , ("positive"       , "0 >")
  , ("negative"       , "0 <")
  , ("prime", "2 [[dup * >] nullary [rem 0 >] dip and] [succ] while dup * < ")
  , ("fact"           , "[1 1] dip [dup [*] dip succ] times pop")
  , ("fib"            , "[1 0] dip [swap [+] unary] times popd")
  , ("nfib"           , "[1 1] dip [dup [+ succ] dip swap] times pop")
  , ("gcd"            , "[0 >] [dup rollup rem] while pop")
  , ("fahrenheit"     , "9 * 5 / 32 + ")
  , ("celsius"        , "32 - 5 * 9 /")
  , ("pi"             , "3.14159265")
  , ("radians"        , "pi * 180 /")
  , ("set-var"        , "swap unitlist define")
  , ( "dictionary"
    , "dup size 2 / [ dup dup second swap first set-var 2 drop ] times pop"
    )
  , ( "defines"
    , "dup size 2 / [ dup dup first swap second define 2 drop ] times pop"
    )
  , ("putchars"    , "[putch] step")
  , ("putstrings"  , "[putchars] step")
  , ("current-path", " \"\" ")
  , ("libload"     , "current-path swap concat \".pless\" concat _libopen")
  , ("run-tests"   , "current-path swap concat \".pless\" concat _runtests")
  , ( "s-to-list"
    , "dup size [uncons] times pop stack dup size 1 - [rolldown pop] times swap pop reverse"
    )
  , ( "unlist"
    , "dup size dup \"_sizeUnlist\" set-var [ uncons ] times pop _sizeUnlist"
    )
  , ( "_flatten"
    , "dup size \"_sizeFlatten\" set-var [ [ unlist ] [ 1 ] iflist ] step [ ] _sizeFlatten [ swap [ cons ] times ] times"
    )
  , ("flatten"    , "[ [ list? ] filter size 0 = ] [ ] [ _flatten ] tailrec")
  , ("cartproduct", "[[]] dip2 [pairlist swap [swons] dip] pairstep ")
  , ( "max"
    , "dup first \"m_\" set-var [][[m_ >] [\"m_\" set-var] [pop] ifte] fold pop m_"
    )
  , ("sindeg", "radians sin")
  , ("cosdeg", "radians cos")
  , ("tandeg", "radians tan")
  , ( "assert"
    , "\"_testSB\" set-var \"_testCase\" set-var _testCase eval \"_testResult\" set-var newstack [ _testSB _testResult = ] [ \"TEST OK = \" putchars _testCase put tx ] [ \"TEST FAILED (expected value = \" putchars _testSB put \", actual value = \" putchars _testResult put \")\n\" putchars \"Failed expression = \" putchars _testCase put tx ] ifte"
    )
  , ("", "")
  , ("", "")
  , ("", "")
  , ("", "")
  , ("", "")
  ]

-- todo fix reverse once string are implemented
-- , ( "variance"
-- , "0.0 swap dup [sum] [size] cleave dup [/ [- dup * +] cons step] dip pred /"
-- )


--    , (""        , "")

-- original definitions
-- , ("reverse" , "[] swap reverse'")
-- , ("reverse'", "[uncons [swons] dip reverse'] [pop] branch")
  -- , ("dig1"    , "swap")
  -- , ("dig2"    , "unitlist cons dip")
  -- , ("dig3"    , "unitlist cons cons dip")
  -- , ("bury1"   , "swap")
  -- , ("bury3"   , "[unitlist cons cons] dip swap exec")
  -- , ("has"     , "swap in")
  -- , ("at"      , "[[rest] dip 1 - at] [pop first] branch")
  -- , ("of"      , "swap at")
  -- , ("primrec" , "dig2 primrec' popd")
  -- , ("primrec'", "[bury2 over2 1 - primrec' dig2 over2 exec] [pop swap exec] branch")
  -- , ("primrec2", "primrec2' [pop2] dip")
  -- , ( "primrec2'"
  --   , "[pop2] [[dup 1 -] dip2 primrec2' [pop] dip2 over2 over2 exec] [swap exec] ifte"
  --   )
  -- , ("primrec3", "primrec3' [pop3] dip")
  -- , ( "primrec3'"
  --   , "over2 [1 - over2 over2 primrec3' [pop3] dip over3 over2 exec] [pop over exec] branch"
  --   )
  -- , ("nub"       , "[] swap [[has] [pop] [swons] ifte] step")
  -- , ("nub2"      , "[] [[has] [pop] [swons] ifte] fold")
  -- , ("rollup"    , "rolldown rolldown")
  -- , ("step"             , "[pop] [[uncons] dip dup dip2 step ] [pop2] ifte")
  -- , ("map"              , "[] rollup [swons] concat step reverse")
  -- , ("map"              , "[] rollup [swons] concat step reverse")
  -- , ("filter"           , "[] rollup [[swons] [pop] ifte] cons step reverse")
  -- , ("unary2"           , "[unary] cons dup dip dip")
  -- , ("negate"  , "[[false] [true] ifte] cons")
  -- , ("tailrec"       , "dup3 [tailrec] cons cons cons concat ifte")
  -- , ("times"   , "[pop] [[pred] dip dup dip2 times] [pop2] ifte")





























