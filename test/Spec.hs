module Main where

import           CoreLibrary
import           Data.Aeson
import qualified Data.Map         as M
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Interpreter
import           Parser
import           PointlessParser
import           Primitives
import           Test.Tasty
import           Test.Tasty.HUnit

-- stack ghci pointless-hs:pointless-hs-test

sKeep01 :: String
sKeep01 = "\"aaa\" [1.1] def aaa aaa [] cons cons \"\" "

sKeep02 :: String
sKeep02 = "\"ab\nc\" putchars ."

sKeep03 :: String
sKeep03 = "10 \'\n\' putch . "

runQuot :: String -> Lang
runQuot s = runQuotation qs lang
  where
    (qs, _) = head $ parse nakedQuotations s
    defs    = getQuotations coreDefinitions ++ primitives
    lang    = Lang (M.fromList defs) [] [] [] ""

main :: IO ()
main = do

    putStrLn $ show (head $ parse nakedQuotations sKeep03)

    let res = runQuot sKeep03

    putStrLn "\nqs after: = "
    print $ stack res

    putStrLn "\nformatStack res: = "
    print $ formatStack $ stack res

    -- putStrLn "\nvocab:"
    -- let xs = M.toList (vocab lang)
    -- mapM_ print xs

    putStrLn "\njson : "
    T.putStr $ jsonResultsShow res
    putStrLn "\n"

    defaultMain unitTests


unitTests :: TestTree
unitTests = testGroup
    "Pointlees interprter tests"
    [ parsePositiveDouble1
    , parsePositiveDouble2
    , parseNegativeDouble1
    , parseNegativeDouble2
    , parseNumberP1
    , parseNumberP2
    , parseNakedQuotation1
    , parseNakedQuotation2
    , parseNakedQuotation3
    , parseCharP1
    , parseCharP2
    , parseQuotedStringP1
    , parseQuotedStringP2
    , formatStack1
    ]

parsePositiveDouble1 :: TestTree
parsePositiveDouble1 = testCase "parse numberDouble 23"
    $ assertEqual [] 23.0 val
    where (val, _) = head $ parse numberDouble "23"

parsePositiveDouble2 :: TestTree
parsePositiveDouble2 = testCase "parse numberDouble 23.4"
    $ assertEqual [] 23.4 val
    where (val, _) = head $ parse numberDouble "23.4"

parseNegativeDouble1 :: TestTree
parseNegativeDouble1 = testCase "parse numberDouble -23"
    $ assertEqual [] (-23.0) val
    where (val, _) = head $ parse numberDouble "-23"

parseNegativeDouble2 :: TestTree
parseNegativeDouble2 = testCase "parse numberDouble -23.4"
    $ assertEqual [] (-23.4) val
    where (val, _) = head $ parse numberDouble "-23.4"


parseNumberP1 :: TestTree
parseNumberP1 = testCase "parse numberP -23"
    $ assertEqual [] (NumP (-23.0)) val
    where (val, _) = head $ parse numberP "-23"

parseNumberP2 :: TestTree
parseNumberP2 = testCase "parse numberP -23.4"
    $ assertEqual [] (NumP (-23.4)) val
    where (val, _) = head $ parse numberP "-23.4"

parseNakedQuotation1 :: TestTree
parseNakedQuotation1 =
    testCase "parse nakedQuotaions \"-10 10 +\" "
        $ assertEqual [] [NumP (-10.0), NumP 10.0, Symbol "+"] val
    where (val, _) = head $ parse nakedQuotations "-10 10 +"

parseNakedQuotation2 :: TestTree
parseNakedQuotation2 =
    testCase "parse nakedQuotaions \"-10 'a'\" "
        $ assertEqual [] [NumP (-10.0), Chr 'a'] val
    where (val, _) = head $ parse nakedQuotations "-10 'a'"

parseNakedQuotation3 :: TestTree
parseNakedQuotation3 =
    testCase "parse nakedQuotaions \"'a' [dup 'z'] i 'b'\" " $ assertEqual
        []
        [Chr 'a', Quot [Symbol "dup", Chr 'z'], Symbol "i", Chr 'b']
        val
    where (val, _) = head $ parse nakedQuotations "'a' [dup 'z'] i 'b'"


parseCharP1 :: TestTree
parseCharP1 = testCase "parse charP \'z\'" $ assertEqual [] (Chr 'z') val
    where (val, _) = head $ parse charP "\'z\'"

parseCharP2 :: TestTree
parseCharP2 = testCase "parse charP \'$\'" $ assertEqual [] (Chr '$') val
    where (val, _) = head $ parse charP "\'$\'"


parseQuotedStringP1 :: TestTree
parseQuotedStringP1 = testCase "parse parsequotedStringP \"abc\""
    $ assertEqual [] (Str "abc") val
    where (val, _) = head $ parse quotedStringP "\"abc\""

parseQuotedStringP2 :: TestTree
parseQuotedStringP2 = testCase "parse parsequotedStringP \"\""
    $ assertEqual [] (Str "") val
    where (val, _) = head $ parse quotedStringP "\"\""

formatStack1 :: TestTree
formatStack1 = testCase "formatStack after running s6"
    $ assertEqual [] "\"\"\n[ 1.100000 1.100000 ]\n" val
    where val = formatStack $ stack $ runQuot sKeep01

















































































