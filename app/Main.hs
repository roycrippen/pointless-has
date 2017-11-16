module Main where

import qualified Data.Map        as Map
import           Interpreter     (Stack, Value (..), WordP (..), formatStack,
                                  runQuotation)
import           Parser
import           PointlessParser (program)
import           Primitives      (primitives)

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
    (definitions, quotations) <- getProgram
    let s = runQuotation quotations (Map.fromList definitions) []
    if null s
    then return ()
    else do
        putStrLn "Residual stack (top to bottom):\n"
        putStrLn $ formatStack s



