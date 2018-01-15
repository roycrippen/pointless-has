module Main where

import           Parsing

main :: IO ()
main = do
  putStrLn "Maybe parsre..."

  putStrLn "\nparser: item"
  print $ parse item "abc"

  putStrLn "\nparser: nat"
  print $ parse nat "123"

  putStrLn "\nparser: nat"
  print $ parse nat "-123"

  putStrLn "\nparser: int"
  print $ parse int "123"

  putStrLn "\nparser: int"
  print $ parse int "-123"

  putStrLn "\neval: (2 + 3 * (2 * 3)) == 20"
  print $ eval "(2 + 3 * (2 * 3))"


