module Main where

import qualified Data.Map        as Map
import           Interpreter
import           Parser
import           PointlessParser
import           Primitives

getProgram :: IO ([(String, WordP)], Stack)
getProgram = do
    source <- readFile "data/test.joy"
    let ((definitions, quotations), _) = head $ parse program source
    return  (primitives ++ definitions, quotations)

stackT :: Stack
stackT = [Quot [Number 1.0,Number 2.0],Number 3.0]

sourceT :: String
sourceT = " (*start*)  DEFINE dup' == dup ; DEFINE pop' == pop ; # aaa \n  # bbb \n (*ccc*) # ddd \n DEFINE roy' == roy ; (*eee*)"


main :: IO ()
main = do
    -- let a@(_, a') = head $ parse (many definition) sourceT
    --     b@(_, b') = head $ parse (comments) a'
    --     -- c@(_, _) = head $ parse comment b'

    -- print a
    -- print b
    -- -- print c




    (definitions, quotations) <- getProgram
    -- mapM_ (\(s, w) -> putStrLn $ s ++ " == " ++ formatWordAST w ) definitions
    -- mapM_ print definitions
    putStrLn "\n"
    mapM_ print quotations
    putStrLn "\n"


    let s = runQuotation quotations (Map.fromList definitions) []
    if null s
    then return ()
    else do
        putStrLn "Residual stack (top to bottom):\n"
        putStrLn $ formatStack s





