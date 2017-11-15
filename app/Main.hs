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
        combined = primitives ++ ds
        optimized = optimizeVocabulary combined
    mapM_ print combined
    mapM_ print optimized
    runQuotation qs (Map.fromList optimized) []
    -- runQuotation qs (Map.fromList combined) []

main :: IO ()
main = do
    s <- runJoy "data/test.joy"
    if null s
      then return ()
      else do putStrLn "Residual stack (top to bottom):"
              dumpStack s


