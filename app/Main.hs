module Main where

import qualified Data.Map        as Map
import           Interpreter
import           Parser
import           PointlessParser
import           Primitives

runJoy :: String -> IO Stack
runJoy fname = do
    source <- readFile fname
    let ((definitions, quotations), _) = head $ parse program source
        combinedFunctions = primitives ++ definitions
    runQuotation quotations (Map.fromList combinedFunctions) []

programT :: IO ([(String, WordP)], Stack)
programT = do
    source <- readFile "data/test.joy"
    let ((ds, qs), _) = head $ parse program source
    return  (primitives ++ ds, qs)

stackT :: Stack
stackT = [Quot [Number 1.0,Number 2.0],Number 3.0]

sourceT :: String
sourceT = "  DEFINE pop' == pop ; DEFINE dup' == dup ; \n\n   (* aksldjkasjdkl \n\n *) \n\n 2 3 pop stack . "

main :: IO ()
main = do
    -- (vs, qs) <- programT
    -- mapM_ print qs
    -- mapM_ print vs
    -- mapM_ (\(s, w) -> putStrLn $ s ++ " == " ++ formatWordAST w ) vs
    -- print stackT

    -- let a@(_, a') = head $ parse (many definition) sourceT
    --     b@(_, b') = head $ parse comment a'
    --     c@(_, _) = head $ parse nakedQuotations b'

    -- print a
    -- print b
    -- print c

    -- s <- runJoy "data/test.joy"
    s <- runJoy "data/test.joy"
    if null s
    then return ()
    else do
        putStrLn "Residual stack (top to bottom):"
        dumpStack s




