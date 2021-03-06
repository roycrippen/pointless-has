module Core
  ( coreDefinitions
  )
where

import qualified Data.Map                      as M
                                                          ( Map
                                                          , fromList
                                                          )
import           Interpreter                              ( WordP(..)
                                                          , Vocabulary
                                                          )
import           Parser                                   ( parse )
import           PointlessParser                          ( nakedQuotations )
import           Primitives

coreDefinitions :: Vocabulary
coreDefinitions = M.fromList $ getQuotations coreLibrary ++ primitives

getQuotation :: (String, String) -> (String, WordP)
getQuotation (name, qs) = (name, Quotation q)
  where (q, _) : _ = parse nakedQuotations qs

getQuotations :: [(String, String)] -> [(String, WordP)]
getQuotations = map getQuotation

primitives :: [(String, WordP)]
primitives =
  [ ("pop"      , Function pop)
  , ("dup"      , Function dup)
  , ("dip"      , Function dip)
  , ("cons"     , Function cons)
  , ("uncons"   , Function uncons)
  , ("concat"   , Function concatP)
  , ("size"     , Function size)
  , ("+"        , Function plus)
  , ("-"        , Function minus)
  , ("*"        , Function $ arithMulDiv (*))
  , ("/"        , Function $ arithMulDiv (/))
  , ("%"        , Function $ arithMulDiv truncMod)
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
  , ("number?"  , Function isNumber)
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
  , ("sqrt"     , Function sqrtP)
  , ("sin"      , Function sinP)
  , ("cos"      , Function cosP)
  , ("tan"      , Function tanP)
  , ("_libopen" , Function _libopen)
  , ("_runtests", Function _runtests)
  ]
    -- , ("binrec"   , Function binrec)

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
  , ("nullary", "stack [exec] dip cons unstack swap pop")
  , ("unary", "stack [exec] dip cons unstack [pop2] dip")
  , ("binary", "stack [exec] dip cons unstack [pop3] dip")
  , ("exec2"   , "[dip] dip exec")
  , ("unary2"  , "[unary  ] cons dup exec2")
  , ("infra", "swons [stack] dip cons unstack [exec stack] dip cons unstack")
  , ("cleave"  , "[dup] dip2 swap dip2 exec")
  , ("branch"  , "[] rollup ifte")
  , ("in", "swap [=] cons filter size [1 >=] [true] [false] ifte swap pop")
  , ("times", "[ pop not ] [ pop2 ] [ [ pred ] dip dup dip2 ] tailrec")
  , ("repeat"  , "[swons] cons [] rollup times")
  , ("step", "[null2] [pop2] [[uncons] dip dup dip2] tailrec")
  , ("map", "swap [[]] [\"\"] iflist swap rolldown [swons] concat step reverse")
  , ("fold"    , "swapd step")
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
  , ("zip", "[null2] [pop2 []] [uncons2] [[pairlist] dip cons] linrec")
  , ( "from-to"
    , "unitlist [pop pop] swoncat [>] swap [[dup succ] dip] [cons] linrec"
    )
  , ("from-to-list"  , "[] from-to")
  , ("from-to-string", "\"\" from-to")
  , ("tailrec"       , "[] linrec")
  , ("split", "dup2 filter rollup [ not ] concat filter")
  , ("pairstep", "[dupd] swoncat [step pop] cons cons step")
  , ("mapr", "[[null] [] [uncons]] dip [dip cons] cons linrec")
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
  , ( "variance"
    , "0.0 swap dup [sum] [size] cleave dup [/ [- dup * +] cons step] dip pred /"
    )
  , ("while", "swap [not] concat [] rolldown tailrec")
  , ("to-upper"  , "['a' >= ] [32 -] when")
  , ("to-lower"  , "['a' <  ] [32 +] when")
  , ("positive"  , "0 >")
  , ("negative"  , "0 <")
  , ("prime", "2 [[dup * >] nullary [rem 0 >] dip and] [succ] while dup * < ")
  , ("fact", "[1 1] dip [dup [*] dip succ] times pop")
  , ("fib", "[1 0] dip [swap [+] unary] times popd")
  , ("nfib", "[1 1] dip [dup [+ succ] dip swap] times pop")
  , ("gcd"       , "[0 >] [dup rollup rem] while pop")
  , ("fahrenheit", "9 * 5 / 32 + ")
  , ("celsius"   , "32 - 5 * 9 /")
  , ("pi"        , "3.14159265")
  , ("radians"   , "pi * 180 /")
  , ("set-var"   , "swap unitlist define")
  , ( "dictionary"
    , "dup size 2 / [ dup dup second swap first set-var 2 drop ] times pop"
    )
  , ( "defines"
    , "dup size 2 / [ dup dup first swap second define 2 drop ] times pop"
    )
  , ("putchars"    , "[putch] step")
  , ("putstrings"  , "[putchars] step")
  , ("current-path", " \"\" ")
  , ("libload", "current-path swap concat \".pless\" concat _libopen")
  , ("run-tests", "current-path swap concat \".pless\" concat _runtests")
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

-- todo fix reverse once string are implemnented


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






























