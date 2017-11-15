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
    source <- readFile "data/debug.joy"
    let ((ds, qs), _) = head $ parse program source
    return  (primitives ++ ds, qs)

stackT :: Stack
stackT = [Quot [Number 1.0,Number 2.0],Number 3.0]

main :: IO ()
main = do
    -- (vs, qs) <- programT
    -- mapM_ print qs
    -- mapM_ print vs
    -- print stackT

    s <- runJoy "data/test.joy"
    if null s
    then return ()
    else do
        putStrLn "Residual stack (top to bottom):"
        dumpStack s



