module Main where

import           CoreLibrary
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Map             as M
import           Interpreter
import           Parser
import           PointlessParser
import           Primitives
import           Test.Tasty
import           Test.Tasty.HUnit


-- stack ghci pointless-hs:pointless-hs-test

s1 :: String
s1
    = " (*aaa*) DEFINE stack' == stack ; \n\
    \ DEFINE neg' == neg ; # bbb  \n\
    \ # ccc \n\
    \ DEFINE map' == map ; \n\
    \ 1 2 3 stack' [neg'] map . \n\
    \ dup . . "

s2 :: String
s2 = " \"a\" uncons "

s3 :: String
s3 = "[1 2 ] [3 4] zip"

s4 :: String
s4 = "['a' 'b' 'c'] [to-upper] map"

s6 :: String
s6 = "\"aaa\" [1.1] def aaa aaa aaa . . . pop aaa  "

s5 :: String
s5 = "DEFINE to-upper' == ['a' >= ] [32 -] when ; 'a' to-upper'"

xs' :: [String]
xs' = go 0 []
    where
        go n xs = if n == 10000 then xs else  go (n+1) ("kdshjhkjashdjhsdjk asdkjkjasdhjasdjkasjdhjahsdkj" : xs)

main :: IO ()
main = do

    -- let ((ds, qs), _) = head $ parse program s5
    --     defs          = getQuotations coreDefinitions ++ primitives ++ ds
    --     lang          = Lang (M.fromList defs) [] [] []
    --     result        = runQuotation qs lang

    -- putStrLn "\nds = "
    -- print ds

    -- putStrLn "\nqs = "
    -- print qs

    -- putStrLn "\nbefore:"
    -- putStrLn $ jsonResultsShow lang

    -- putStrLn "\nafter:"
    -- putStrLn $ jsonResultsShow result


    let (qs, _) = head $ parse nakedQuotations s6
        defs    = getQuotations coreDefinitions ++ primitives
        lang    = Lang (M.fromList defs) [] [] []
        res  = runQuotation qs lang

    putStrLn "\nqs before: = "
    print qs

    putStrLn "\nqs after: = "
    print $ stack res

    putStrLn "\nformatStack qs: = "
    print $ formatStack $ stack res

    putStrLn "\nbefore:"
    putStrLn $ jsonResultsShow lang

    putStrLn "\nafter:"
    putStrLn $ jsonResultsShow res

    -- putStrLn "\nvocab:"
    -- let xs = M.toList (vocab lang)
    -- mapM_ print xs

    putStrLn "\njson result: "
    B.putStr $ encode (result res)
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
    testCase "parse nakedQuotaions \"'a' [dup 'z'] i 'b'\" "
        $ assertEqual [] [Chr 'a',  Quot [Symbol "dup", Chr 'z'], Symbol "i", Chr 'b'] val
    where (val, _) = head $ parse nakedQuotations "'a' [dup 'z'] i 'b'"


parseCharP1 :: TestTree
parseCharP1 = testCase "parse charP \'z\'"
    $ assertEqual [] (Chr 'z') val
    where (val, _) = head $ parse charP "\'z\'"

parseCharP2 :: TestTree
parseCharP2 = testCase "parse charP \'$\'"
    $ assertEqual [] (Chr '$') val
    where (val, _) = head $ parse charP "\'$\'"


parseQuotedStringP1 :: TestTree
parseQuotedStringP1 = testCase "parse parsequotedStringP \"abc\""
    $ assertEqual [] (Str "abc") val
    where (val, _) = head $ parse quotedStringP "\"abc\""

parseQuotedStringP2 :: TestTree
parseQuotedStringP2 = testCase "parse parsequotedStringP \"\""
    $ assertEqual [] (Str "") val
    where (val, _) = head $ parse quotedStringP "\"\""












































































