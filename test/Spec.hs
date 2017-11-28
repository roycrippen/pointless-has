module Main where

import           CoreLibrary
import           Data.Map         as M
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
s2 = "-10 10 +"

main :: IO ()
main = do


    let ((ds, qs), _) = head $ parse program s1
        defs          = getQuotations coreDefinitions ++ primitives ++ ds
        lang          = Lang (M.fromList defs) [] [] []
        result        = runQuotation qs lang

    putStrLn "\n"
    print qs

    putStrLn "before:"
    putStrLn $ jsonResultsShow lang

    putStrLn "\nafter:"
    putStrLn $ jsonResultsShow result

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
    $ assertEqual [] (Number (-23.0)) val
    where (val, _) = head $ parse numberP "-23"

parseNumberP2 :: TestTree
parseNumberP2 = testCase "parse numberP -23.4"
    $ assertEqual [] (Number (-23.4)) val
    where (val, _) = head $ parse numberP "-23.4"

parseNakedQuotation1 :: TestTree
parseNakedQuotation1 =
    testCase "parse nakedQuotaions \"-10 10 +\" "
        $ assertEqual [] [Number (-10.0), Number 10.0, Symbol "+"] val
    where (val, _) = head $ parse nakedQuotations "-10 10 +"














































































