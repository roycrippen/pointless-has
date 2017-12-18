module Core ( coreDefinitions ) where

import           Interpreter     (WordP (..))
import           Parser          (parse)
import           PointlessParser (nakedQuotations)
import           Primitives

coreDefinitions :: [(String, WordP)]
coreDefinitions = getQuotations coreLibrary ++ primitives

getQuotation :: (String, String) -> (String, WordP)
getQuotation (name, qs) = (name, Quotation q)
  where (q, _):_ = parse nakedQuotations qs

getQuotations :: [(String, String)] -> [(String, WordP)]
getQuotations = map getQuotation

primitives :: [(String, WordP)]
primitives =
    [ ("pop"      , Function pop)
    , ("dup"      , Function dup)
    , ("cons"     , Function cons)
    , ("uncons"   , Function uncons)
    , ("concat"   , Function concatP)
    , ("+"        , Function plus)
    , ("-"        , Function minus)
    , ("*"        , Function $ arithMulDiv (*))
    , ("/"        , Function $ arithMulDiv (/))
    , ("%"        , Function $ arithMulDiv truncMod)
    , ("="        , Function $ comparison (==))
    , ("<="       , Function $ comparison (<=))
    , (">="       , Function $ comparison (>=))
    , ("<"        , Function $ comparison (<))
    , (">"        , Function $ comparison (>))
    , ("and"      , Function $ logic (&&))
    , ("or"       , Function $ logic (||))
    , ("null"     , Function lnot)
    , ("stack"    , Function stackP)
    , ("unstack"  , Function unstack)
    , ("."        , Function printVal)
    , ("put"      , Function put)
    , ("putch"    , Function putch)
    , ("dip"      , Function dip)
    , ("x"        , Function xP)
    , ("i"        , Function iP)
    , ("ifte"     , Function ifThenElse)
    , ("list"     , Function list)
    , ("linrec"   , Function linrec)
    , ("define"   , Function define)
    , ("libload"  , Function libload)
    ]

coreLibrary :: [(String, String)]
coreLibrary =
  [ ("true"          , "1")
  , ("false"         , "0")
  , ("rem"           , "%")
  , ("not"           , "null")
  , ("unit"          , "[] cons")
  , ("unitlist"      , "[] cons")
  , ("swap"          , "unit dip")
  , ("pop2"          , "pop pop")
  , ("pop3"          , "pop pop pop")
  , ("popd"          , "[pop] dip")
  , ("dupd"          , "[dup] dip")
  , ("swapd"         , "[swap] dip")
  , ("swons"         , "swap cons")
  , ("first"         , "uncons pop")
  , ("rest"          , "uncons swap pop")
  , ("over"          , "[dup] dip swap")
  , ("over2"         , "[over] dip swap")
  , ("over3"         , "[over2] dip swap")
  , ("dup2"          , "over over")
  , ("dup3"          , "over2 over2 over2")
  , ("dip1"          , "dip")
  , ("dip2"          , "[dip] cons dip")
  , ("dip3"          , "[dip] cons [dip] cons dip")
  , ("nullary"       , "stack [i] dip cons unstack swap pop")
  , ("unary"         , "stack [i] dip cons unstack [pop2] dip")
  , ("binary"        , "stack [i] dip cons unstack [pop3] dip")
  , ("i2"            , "[dip] dip i")
  , ("unary2"        , "[unary  ] cons dup i2")
  , ("infra"         , "swons [stack] dip cons unstack [i stack] dip cons unstack")
  , ("cleave"        , "[dup] dip2 swap dip2 i")
  , ("branch"        , "[] rollup ifte")
  , ("in"            , "swap [=] cons filter size [1 >=] [true] [false] ifte swap pop")
  , ("times"         , "[pop] [[pred] dip dup dip2 times] [pop2] ifte")
  , ("repeat"        , "[swons] cons [] rollup times")
  , ("step"          , "[null2] [pop2] [[uncons] dip dup dip2] tailrec")
  , ("size"          , "0 swap [pop succ] step")
  , ("map"           , "swap [[]] [\"\"] iflist swap rolldown [swons] concat step reverse")
  , ("fold"          , "swapd step")
  , ( "filter"       , "swap [[]] [\"\"] iflist swap rolldown [[swons] [pop] ifte] cons step reverse")
  , ("even"          , "2 % 0 =")
  , ("odd"           , "even not")
  , ("swoncat"       , "swap concat")
  , ("tailrec"       , "dup3 [tailrec] cons cons cons concat ifte")
  , ("when"          , "[] ifte")
  , ("unles"         , "[] swap ifte")
  , ("neg"           , "-1 *")
  , ("abs"           , "[0 <] [neg] when")
  , ("keep"          , "dupd dip")
  , ("rolldown"      , "swapd swap")
  , ("rollup"        , "[[] cons cons] dip swap i")
  , ("iflist"        , "[list] rollup ifte")
  , ("unswons"       , "uncons swap")
  , ("shunt"         , "[swons] step")
  , ("reverse"       , "[[]] [\"\"] iflist swap shunt")
  , ("reversed"      , "[reverse] dip")
  , ("sum"           , "0 [+] fold")
  , ("product"       , "1 [*] fold")
  , ("succ"          , "1 +")
  , ("pred"          , "1 -")
  , ("newstack"      , "[] unstack")
  , ("sequor"        , "[pop true] swap ifte")
  , ("sequand"       , "[pop false] ifte")
  , ("dipd"          , "[dip] cons dip")
  , ("cleave"        , "[dup] dip2 swap dip2 i")
  , ("true"          , "1")
  , ("false"         , "0")
  , ("truth"         , "true")
  , ("falsity"       , "false")
  , ("conjoin"       , "[[false] ifte] cons cons ")
  , ("disjoin"       , "[ifte] cons [true] swons cons")
  , ("call"          , "[] cons i")
  , ("drop"          , "[rest] times")
  , ("pairlist"      , "[] cons cons")
  , ("unpair"        , "uncons uncons pop")
  , ("second"        , "rest first")
  , ("third"         , "rest rest first")
  , ("fourth"        , "3 drop first")
  , ("fifth"         , "4 drop first")
  , ("nulld"         , "[null] dip")
  , ("consd"         , "[cons] dip")
  , ("swonsd"        , "[swons] dip")
  , ("unconsd"       , "[uncons] dip")
  , ("unswonsd"      , "[unswons] dip")
  , ("firstd"        , "[first] dip")
  , ("restd"         , "[rest] dip")
  , ("secondd"       , "[second] dip")
  , ("thirdd"        , "[third] dip")
  , ("null2"         , "nulld null or")
  , ("cons2"         , "swapd cons consd")
  , ("uncons2"       , "unconsd uncons swapd")
  , ("swons2"        , "swapd swons swonsd")
  , ("unswons2"      , "[unswons] dip unswons swapd")
  , ("zip"           , "[null2] [pop2 []] [uncons2] [[pairlist] dip cons] linrec")
  , ( "from-to"      , "[] cons [pop pop] swoncat [>] swap [[dup succ] dip] [cons] linrec")
  , ("from-to-list"  , "[] from-to")
  , ("from-to-string", "\"\" from-to")
  , ("tailrec"       , "[] linrec")
  , ("split"         , "dup2 filter rollup [ not ] concat filter")
  , ("pairstep"      , "[dupd] swoncat [step pop] cons cons step")
  , ("mapr"          , "[[null] [] [uncons]] dip [dip cons] cons linrec")
  , ("foldr"         , "[[[null]] dip [] cons [pop] swoncat [uncons]] dip linrec")
  , ("stepr2"        , "[[null2] [pop pop]] dip [dip] cons [dip] cons [uncons2] swoncat tailrec")
  , ("mapr2"         , "[[null2] [pop2 []] [uncons2]] dip [dip cons] cons linrec")
  , ("foldr2"        , "[[ [null2] ] dip [] cons [pop2] swoncat [uncons2] ] dip linrec")
  , ("interleave2"   , "[cons cons] foldr2")
  , ("interleave2list", "[] interleave2 ")
  , ("average"       , "[ sum ] [ size ] cleave / ")
  , ( "variance"     , "0.0 swap dup [sum] [size] cleave dup [/ [- dup * +] cons step] dip pred /")
  , ("while"         , "swap [not] concat [] rolldown tailrec")
  , ("to-upper"      , "['a' >= ] [32 -] when")
  , ("to-lower"      , "['a' <  ] [32 +] when")
  , ("positive"      , "0 >")
  , ("negative"      , "0 <")
  , ("prime"         , "2 [[dup * >] nullary [rem 0 >] dip and] [succ] while dup * < ")
  , ("fact"          , "[1 1] dip [dup [*] dip succ] times pop")
  , ("fib"           , "[1 0] dip [swap [+] unary] times popd")
  , ("nfib"          , "[1 1] dip [dup [+ succ] dip swap] times pop")
  , ("gcd"           , "[0 >] [dup rollup rem] while pop")
  , ("fahrenheit"    , "9 * 5 / 32 + ")
  , ("celsius"       , "32 - 5 * 9 /")
  , ("pi"            , "3.14159265")
  , ("radians"       , "pi * 180 /")
  , ("set-var"       , "swap [] cons define")
  , ("dictionary"    , "dup size 2 / [ dup dup second swap first set-var 2 drop ] times pop")
  , ("defines"       , "dup size 2 / [ dup dup first swap second define 2 drop ] times pop")
  , ("putchars"      , "[putch] step")
  , ("putstrings"    , "[putchars] step")
  , (""  , "")
  , (""  , "")
  , (""  , "")
  , (""  , "")
  , (""  , "")
  , (""  , "")
  , (""  , "")
  ]

-- todo fix reverse once string are implemnented

--    , (""        , "")

-- original definitions
-- , ("reverse" , "[] swap reverse'")
-- , ("reverse'", "[uncons [swons] dip reverse'] [pop] branch")
  -- , ("dig1"    , "swap")
  -- , ("dig2"    , "[] cons cons dip")
  -- , ("dig3"    , "[] cons cons cons dip")
  -- , ("bury1"   , "swap")
  -- , ("bury3"   , "[[] cons cons cons] dip swap i")
  -- , ("has"     , "swap in")
  -- , ("at"      , "[[rest] dip 1 - at] [pop first] branch")
  -- , ("of"      , "swap at")
  -- , ("primrec" , "dig2 primrec' popd")
  -- , ("primrec'", "[bury2 over2 1 - primrec' dig2 over2 i] [pop swap i] branch")
  -- , ("primrec2", "primrec2' [pop2] dip")
  -- , ( "primrec2'"
  --   , "[pop2] [[dup 1 -] dip2 primrec2' [pop] dip2 over2 over2 i] [swap i] ifte"
  --   )
  -- , ("primrec3", "primrec3' [pop3] dip")
  -- , ( "primrec3'"
  --   , "over2 [1 - over2 over2 primrec3' [pop3] dip over3 over2 i] [pop over i] branch"
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
  --



