module CoreLibrary
    ( getQuotations
    , coreDefinitions
    ) where

import           Interpreter     (WordP (..))
import           Parser          (parse)
import           PointlessParser (nakedQuotations)

getQuotation :: (String, String) -> (String, WordP)
getQuotation (name, qs) = (name, Quotation q)
    where q = fst $ head (parse nakedQuotations qs)

getQuotations :: [(String, String)] -> [(String, WordP)]
getQuotations = map getQuotation

coreDefinitions :: [(String, String)]
coreDefinitions =
    [ ("true"   , "1")
    , ("false"  , "0")
    , ("not"    , "null")
    , ("unit"   , "[] cons")
    , ("swap"   , "unit dip")
    , ("pop2"   , "pop pop")
    , ("pop3"   , "pop pop pop")
    , ("popd"   , "[pop] dip")
    , ("dupd"   , "[dup] dip")
    , ("swapd"  , "[swap] dip")
    , ("swons"  , "swap cons")
    , ("first"  , "uncons pop")
    , ("rest"   , "uncons swap pop")
    , ("over"   , "[dup] dip swap")
    , ("over2"  , "[over] dip swap")
    , ("over3"  , "[over2] dip swap")
    , ("dup2"   , "over over")
    , ("dup3"   , "over2 over2 over2")
    , ("dip1"   , "dip")
    , ("dip2"   , "[dip] cons dip")
    , ("dip3"   , "[dip] cons [dip] cons dip")
    , ("dig1"   , "swap")
    , ("dig2"   , "[] cons cons dip")
    , ("dig3"   , "[] cons cons cons dip")
    , ("bury1"  , "swap")
    , ("bury2"  , "[[] cons cons] dip swap i")
    , ("bury3"  , "[[] cons cons cons] dip swap i")
    , ("nullary", "stack [i] dip cons unstack swap pop")
    , ("unary"  , "stack [i] dip cons unstack [pop2] dip")
    , ("binary" , "stack [i] dip cons unstack [pop3] dip")
    , ("unary2" , "[unary] cons dup dip dip")
    , ("infra"  , "swons [stack] dip cons unstack [i stack] dip cons unstack")
    , ("cleave" , "[dup] dip2 swap dip2 i")
    , ("branch" , "[] bury2 ifte")
    , ("in", "swap [=] cons filter size [1 >=] [true] [false] ifte swap pop")
    , ("has"    , "swap in")
    , ("at"     , "[[rest] dip 1 - at] [pop first] branch")
    , ("of"     , "swap at")
    , ("times"  , "[pop] [[1 -] dip dup dip2 times] [pop2] ifte")
    , ("repeat" , "[swons] cons [] bury2 times")
    , ("primrec", "dig2 primrec' popd")
    , ( "primrec'"
      , "[bury2 over2 1 - primrec' dig2 over2 i] [pop swap i] branch"
      )
    , ("primrec2", "primrec2' [pop2] dip")
    , ( "primrec2'"
      , "[pop2] [[dup 1 -] dip2 primrec2' [pop] dip2 over2 over2 i] [swap i] ifte"
      )
    , ("primrec3", "primrec3' [pop3] dip")
    , ( "primrec3'"
      , "over2 [1 - over2 over2 primrec3' [pop3] dip over3 over2 i] [pop over i] branch"
      )
    , ("step"      , "[pop] [[uncons] dip dup dip2 step ] [pop2] ifte")
    , ("size"      , "0 swap [pop 1 +] step")
    , ("map"       , "[] bury2 [swons] concat step reverse")
    , ("fold"      , "swapd step")
    , ("filter"    , "[] bury2 [[swons] [pop] ifte] cons step reverse")
    , ("nub"       , "[] swap [[has] [pop] [swons] ifte] step")
    , ("nub2"      , "[] [[has] [pop] [swons] ifte] fold")
    , ("even"      , "2 % 0 =")
    , ("odd"       , "even not")
    , ("swoncat"   , "swap concat")
    , ("tailrec"   , "dup3 [tailrec] cons cons cons concat ifte")
    , ("when"      , "[] ifte")
    , ("unles"     , "[] swap ifte")
    , ("neg"       , "0 swap -")
    , ("abs"       , "dup 0  [<] [neg] when")
    , ("keep"      , "dupd dip")
    , ("rolldown"  , "swapd swap")
    , ("rollup"    , "rolldown rolldown")
    , ("iflist"    , "[list] rollup ifte")
    , ("unswons"   , "uncons swap")
    , ("shunt"     , "[swons] step")
    , ("reverse"   , "[[]] [[]] iflist swap shunt")
    , ("reversed"  , "[reverse] dip")
    , ("sum"       , "0 [+] fold")
    , ("product"   , "1 [*] fold")
    , ("succ"      , "1 +")
    , ("pred"      , "1 -")
    , ("newstack"  , "[] unstack")
    , ("factortial", "[dup 1 - factorial *] [pop 1] branch")
    , ("fib"       , "[1 0] dip [swap [+] unary] times popd")
    ]

-- todo fix reverse once string are implemnented

--    , (""        , "")

-- original definitions
-- , ("reverse" , "[] swap reverse'")
-- , ("reverse'", "[uncons [swons] dip reverse'] [pop] branch")



































