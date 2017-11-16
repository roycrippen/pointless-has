-- module Main where

-- import           Interpreter     (WordP)
import           Interpreter     (formatWordAST)
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
  let ((defs, quots), _) = head $ parse program sourceT

  putStrLn "\n"
  mapM_ (\(s, w) -> putStrLn $ s ++ " == " ++ formatWordAST w ) defs
  putStrLn "\n"

  mapM_ print defs
  putStrLn "\n"

  mapM_ print quots
  putStrLn "\n"
