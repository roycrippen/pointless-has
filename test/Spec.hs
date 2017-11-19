-- module Main where

-- import           Interpreter     (WordP)
import           CoreLibrary
import           Data.Map        as M
import           Interpreter
import           Parser
import           PointlessParser
import           Primitives


-- stack ghci pointless-hs:pointless-hs-test

sourceT :: String
sourceT
    = " (*aaa*) DEFINE pop' == pop ; \n\
    \ DEFINE dup' == dup ; # bbb  \n\
    \ # ccc \n\
    \ DEFINE fact == [dup 1 - fact *] [pop 1] branch ; \n\
    \ 1 2 3 dup' dup' pop' . "

sourceT2 :: String
sourceT2 = "[] 1 swap"

main :: IO ()
main = do

    -- let ((vocabulary, quots), _) = head $ parse program sourceT
    --     aStack = [ Number 1.0, Number 2.0, Symbol "+", Quot [ Symbol "dup", Number 1.0, Symbol "+" ] ]
    --     lang = Lang (M.fromList vocabulary) aStack ["display 1", "display 2"] ["error 1", "error 2"]

    let (quots, _) = head $ parse (many instruction) sourceT2
        defs       = (getQuotations coreDefinitions) ++ primitives
        lang       = Lang (M.fromList defs) [] [] []
        result     = runQuotation quots lang


    putStrLn "\n"
    print quots

    putStrLn "before:\n"
    putStrLn $ jsonResultsShow lang

    putStrLn "after:\n"
    putStrLn $ jsonResultsShow result

            -- putStrLn "\n"
    -- putStrLn $ jsonResultsShow lang

    -- putStrLn "\n"
    -- mapM_ (\(s, w) -> putStrLn $ s ++ " == " ++ formatWordAST w) vocabulary

    -- putStrLn "\n"
    -- mapM_ print vocabulary

    -- putStrLn "\n"
    -- mapM_ print quots

    -- let quotS = parse nakedQuotations "[dup 1 - fact *] [pop 1] branch"
    -- print quotS

    -- let aaa = getQuotations coreDefinitions
    -- print aaa































