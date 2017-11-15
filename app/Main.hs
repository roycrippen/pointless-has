module Main where

import qualified Data.Map        as Map
import           Interpreter
import           Optimizer
import           Parser
import           PointlessParser
import           Primitives

runJoy :: String -> IO (Stack)
runJoy fname = do
    source <- readFile fname
    let ((ds, qs), _) = head $ parse program source
    mapM_ print primitives
    mapM_ print ds
    mapM_ print qs
    runQuotation qs (Map.fromList $ optimizeVocabulary $ primitives ++ ds) []

main :: IO ()
main = do
    s <- runJoy "data/test.joy"
    if null s
      then return ()
      else do putStrLn "Residual stack (top to bottom):"
              dumpStack s


