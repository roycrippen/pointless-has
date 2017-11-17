module Main where

import qualified Data.Map        as M
import           Interpreter     (Stack, WordP (..), formatStack, runQuotation)
import           Parser
import           PointlessParser (program)
import           Primitives      (primitives)

getProgram :: IO ([(String, WordP)], Stack)
getProgram = do
    source <- readFile "data/test.joy"
    let ((defs, quots), _) = head $ parse program source
    return (primitives ++ defs, quots)

main :: IO ()
main = do
    (vocabulary, quots) <- getProgram
    let s = runQuotation quots (M.fromList vocabulary) []
    if null s
        then return ()
        else do
            putStrLn "Residual stack (top to bottom):\n"
            putStrLn $ formatStack s







