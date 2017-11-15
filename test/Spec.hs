-- module Main where

import           Interpreter     (WordP)
import           Parser          (parse)
import           PointlessParser (definition)

main :: IO ()
main = do
  let ((name, quotation), _) = head $ parse definition " DEFINE pop' == [ pop ]; "
  print ""
  print (name ++ " == " ++ (show quotation))

