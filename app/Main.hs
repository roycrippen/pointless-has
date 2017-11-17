module Main where

import           CoreLibrary     (coreDefinitions, getQuotations)
import qualified Data.Map        as M
import           Interpreter     (Lang (..), Stack, WordP (..), formatStack,
                                  jsonLangShow, runQuotation)
import           Parser
import           PointlessParser (program)
import           Primitives      (primitives)

getProgram :: IO ([(String, WordP)], Stack)
getProgram = do
    source <- readFile "data/test.joy"
    let ((defs, quots), _) = head $ parse program source
        coreLibrary = getQuotations coreDefinitions
    return (primitives ++ coreLibrary ++ defs, quots)

main :: IO ()
main = do
    (vocabulary, quots) <- getProgram
    let lang = runQuotation quots (Lang (M.fromList vocabulary) [] [] [])
        s = stack lang
    if null s
        then return ()
        else do
            putStrLn "Residual stack (top to bottom):\n"
            putStrLn $ formatStack s

    putStrLn $ jsonLangShow lang



