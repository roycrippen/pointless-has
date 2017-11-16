-- module Main where

-- import           Interpreter     (WordP)
import           Data.Map        as M (fromList)
import           Interpreter     (Lang (..), Value (..), formatWordAST,
                                  jsonLangShow)
import           Parser          (parse)
import           PointlessParser (program)


-- stack ghci pointless-hs:pointless-hs-test

sourceT :: String
sourceT =
    " (*aaa*) DEFINE pop' == pop ; \n\
    \ DEFINE dup' == dup ; # bbb  \n\
    \ # ccc \n\
    \ DEFINE fact == [dup 1 - fact *] [pop 1] branch ; \n\
    \ 1 2 3 dup' dup' pop' . "

main :: IO ()
main = do
    let ((vocabulary, quots), _) = head $ parse program sourceT
        aStack = [ Number 1.0, Number 2.0, Symbol "+", Quot [ Symbol "dup", Number 1.0, Symbol "+" ] ]
        lang = Lang (M.fromList vocabulary) aStack ["display 1", "display 2"] ["error 1", "error 2"]

    putStrLn "\n"
    putStrLn $ jsonLangShow lang

    putStrLn "\n"
    mapM_ (\(s, w) -> putStrLn $ s ++ " == " ++ formatWordAST w ) vocabulary

    putStrLn "\n"
    mapM_ print vocabulary

    putStrLn "\n"
    mapM_ print quots

