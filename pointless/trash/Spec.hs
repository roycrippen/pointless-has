module Main where

import qualified Data.Map         as M
import qualified Data.Text.IO     as T
import           Interpreter
import           Parser
import           Primitives
import           SocketServer
import           System.IO.Unsafe (unsafeDupablePerformIO)
import           Test.Tasty
import           Test.Tasty.HUnit

-- stack ghci pointless/test/Spec.hs

sKeep01 :: String
sKeep01 = "\"aaa\" [1.1] define aaa aaa [] cons cons dup "

sKeep02 :: String
sKeep02 = "\"a\\\n\\\nz\" putchars "

s1 :: String
s1 = " 10 \"aaa\" set-var 3 dup "

s1' :: String
s1' = "10 \"aaa\" [ ] cons dip [ ] cons define 3 dup"

s2 = " [1 2 3] size "

s1AstFull :: [ValueP]
s1AstFull =
  [ NumP 10
  , Str "aaa"
  , Quot []
  , Sym "cons"
  , Sym "dip"
  , Quot []
  , Sym "cons"
  , Sym "define"
  , NumP 3
  , Sym "dup"
  ]

testSource :: String
testSource
  = "        \"pl-test\" libload                                                        \
  \                                                                                   \
  \         { [zipped-list last] ['j' 9] assert }                                     \
  \         \"zipped-list\" [ 'a' 'j' from-to-string 0 9 from-to-list zip ] define    \
  \                                                                                   \
  \         { [dt] 0.01 assert                                                        \
  \           [springCoeff] 39.47 assert                                              \
  \           [func] 0.3947 assert                                                    \
  \         }                                                                         \
  \         [ \"dampCoeff\"      [[8.88 12.0 11.11]]                                  \
  \           \"dt\"             [0.01]                                               \
  \           \"gravity\"        [-9.88]                                              \
  \           \"mass\"           [1.00]                                               \
  \           \"springCoeff\"    [39.47]                                              \
  \           \"func\"           [springCoeff dt *]                                   \
  \         ] defines                                                                 \
  \                                                                                   \
  \         { [test-defs pop3 pop2] [8.88 12.0 11.11] assert }                        \
  \         \"test-defs\" [ dampCoeff dt gravity mass springCoeff func ] define       \
  \                                                                                   \
  \         { [maxValue minValue +] 0 assert }                                        \
  \         [ \"maxValue\"  100                                                       \
  \           \"minValue\" -100                                                       \
  \         ] dictionary                                                              \
  \                                                                                   \
  \         10 \"abc_\" set-var tx                                                    \
  \                                                                                   \
  \         $ \"small\" [dup size [2 <] exec] define                                  \
  \                                                                                   \
  \         $ \"qsort\"                                                               \
  \         $   [ [small]                                                             \
  \         $     []                                                                  \
  \         $     [uncons [>] split]                                                  \
  \         $     [swapd cons concat]                                                 \
  \         $     binrec                                                              \
  \         $   ] define   $ slow "

testSource2 :: String
testSource2 = "\"scratch-pad\" run-tests"

getAst :: String -> [ValueP]
getAst s = ast where (ast, _) = head $ parse nakedQuotations s

ioTest :: Lang -> Lang
ioTest lang = unsafeDupablePerformIO $ do
  putStrLn "ioTest:"
  mapM_ putStrLn (result lang)
  return lang { result = [] }

runQuot :: String -> Lang
runQuot s = runQuotation qs (Lang coreDefinitions [] [] "" REPL)
  where (qs, _):_ = parse nakedQuotations s

main :: IO ()
main = do

  let xs  = getAst s1'
      xs' = primitiveAST coreDefinitions xs

  print xs
  print xs'
  print s1AstFull
  putStrLn $ "xs' == s1AstFull: " ++ show (xs' == s1AstFull)
  putStrLn ""



  -- let lang  = Lang coreDefinitions [] ["10", "20"] "" REPL
  --     lang' = ioTest lang
  -- print "done"
  -- print $ result lang'

  defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup
  "Pointlees interprter tests"
  [ parseNumberP1
  , parseNumberP2
  , parseNakedQuotation1
  , parseNakedQuotation2
  , parseNakedQuotation3
  , parseCharP1
  , parseCharP2
  , parseQuotedStringP1
  , parseQuotedStringP2
  , escapeNewLine1
  ]

-- parsePositiveDouble1 :: TestTree
-- parsePositiveDouble1 = testCase "parse numberDouble 23"
--   $ assertEqual [] 23.0 val
--   where (val, _):_ = parse numberDouble "23"

-- parsePositiveDouble2 :: TestTree
-- parsePositiveDouble2 = testCase "parse numberDouble 23.4"
--   $ assertEqual [] 23.4 val
--   where (val, _):_ = parse numberDouble "23.4"

-- parseNegativeDouble1 :: TestTree
-- parseNegativeDouble1 = testCase "parse numberDouble -23"
--   $ assertEqual [] (-23.0) val
--   where (val, _):_ = parse numberDouble "-23"

-- parseNegativeDouble2 :: TestTree
-- parseNegativeDouble2 = testCase "parse numberDouble -23.4"
--   $ assertEqual [] (-23.4) val
--   where (val, _):_ = parse numberDouble "-23.4"

parseNumberP1 :: TestTree
parseNumberP1 = testCase "parse numberP -23" $ assertEqual [] (NumP (-23)) val
  where (val, _):_ = parse numberP "-23"

parseNumberP2 :: TestTree
parseNumberP2 = testCase "parse numberP -23" $ assertEqual [] (NumP (-23)) val
  where (val, _):_ = parse numberP "-23"

parseNakedQuotation1 :: TestTree
parseNakedQuotation1 =
  testCase "parse nakedQuotaions \"-10 10 +\" "
    $ assertEqual [] [NumP (-10), NumP 10, Sym "+"] val
  where (val, _):_ = parse nakedQuotations "-10 10 +"

parseNakedQuotation2 :: TestTree
parseNakedQuotation2 =
  testCase "parse nakedQuotaions \"-10 'a'\" "
    $ assertEqual [] [NumP (-10), Chr 'a'] val
  where (val, _):_ = parse nakedQuotations "-10 'a'"

parseNakedQuotation3 :: TestTree
parseNakedQuotation3 =
  testCase "parse nakedQuotaions \"'a' [dup 'z'] i 'b'\" " $ assertEqual
    []
    [Chr 'a', Quot [Sym "dup", Chr 'z'], Sym "i", Chr 'b']
    val
  where (val, _):_ = parse nakedQuotations "'a' [dup 'z'] i 'b'"


parseCharP1 :: TestTree
parseCharP1 = testCase "parse charP \'z\'" $ assertEqual [] (Chr 'z') val
  where (val, _):_ = parse charP "\'z\'"

parseCharP2 :: TestTree
parseCharP2 = testCase "parse charP \'$\'" $ assertEqual [] (Chr '$') val
  where (val, _):_ = parse charP "\'$\'"


parseQuotedStringP1 :: TestTree
parseQuotedStringP1 = testCase "parse parsequotedStringP \"abc\""
  $ assertEqual [] (Str "abc") val
  where (val, _):_ = parse quotedStringP "\"abc\""

parseQuotedStringP2 :: TestTree
parseQuotedStringP2 = testCase "parse parsequotedStringP \"\""
  $ assertEqual [] (Str "") val
  where (val, _):_ = parse quotedStringP "\"\""

-- formatStack1 :: TestTree
-- formatStack1 = testCase "formatStack after running"
--   $ assertEqual [] ["[ 1.100000 1.100000 ]", "[ 1.100000 1.100000 ]"] val
--   where val = formatStack $ stack $ runQuot sKeep01

escapeNewLine1 :: TestTree
escapeNewLine1 = testCase "escapeNewLine parser test"
  $ assertEqual [] ["a", "", "z"] val
 where
  val = lines (display res)
  res = runQuot sKeep02






































