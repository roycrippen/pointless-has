module Main where

import           CLaSH.Prelude       hiding (many, (++), (<|>))
import           Control.Applicative (Applicative (..), pure)
import           Control.Monad       (Functor (..), Monad (..), ap, liftM, void)
import           Data.Bool
import           Data.Char
import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.List           as L
import           Data.Maybe          (isJust)
import           Data.String
import           Interpreter
import           Parser
import           Text.Read


main :: IO ()
main = do
  putStrLn "Pointless in Clash tests\n"
  parserTests

parserTests :: IO ()
parserTests = do
  let (a1, _):_ = parse numberInt "123 abc"
  putStrLn $  "parse numberInt: " ++ if (a1 == 123) then "OK" else "ERROR"
  putStrLn $ show a1 ++ "\n"

  let (s1, _):_ = parse quotedString "\"hello world\" 123"
  putStrLn $ "parse quotedString: " ++ if (s1 == "hello world") then "OK" else "ERROR"
  putStrLn $ s1 ++ "\n"

  let (c1, _):_ = parse firstLetter "abc"
  putStrLn $ "parse firstLetter: " ++ if (c1 == 'a') then "OK" else "ERROR"
  putStrLn $ show c1 ++ "\n"

  let (c2, _):_ = parse firstLetter "_abc"
  putStrLn $ "parse firstLetter: " ++ if (c2 == '_') then "OK" else "ERROR"
  putStrLn $ show c2 ++ "\n"

  let (c3, _):_ =  parse charP "'z'"
  putStrLn $ "parse charP: " ++ if (c3 == (Chr 'z')) then "OK" else "ERROR"
  putStrLn $ show c3 ++ "\n"

  let (v0, _):_ = parse quotedStringP "\"abc\""
      v0Str =  if (v0 == Str "abc") then "OK" else "ERROR"
  putStrLn $ "parse quotedString: " ++ v0Str
  putStrLn $ show v0 ++ "\n"

  let (v1, _):_ = parse numberP "-23"
      v1Str =  if (v1 == (NumP (-23))) then "OK" else "ERROR"
  putStrLn $ "parse numberP: " ++ v1Str
  putStrLn $ show v1 ++ "\n"

  let (v2, _):_ = parse nakedQuotations "-10 10 +"
      v2Str =  if (v2 == [NumP (-10),NumP 10,Sym "+"]) then "OK" else "ERROR"
  putStrLn $ "parse nakedQuotation: " ++ v2Str
  putStrLn $ show v2 ++ "\n"

  let (v3, _):_ = parse nakedQuotations " 10 \"aaa\" set-var 3 dup "
      v3Str =  if (v3 == [NumP 10,Str "aaa",Sym "set-var", NumP 3, Sym "dup"]) then "OK" else "ERROR"
  putStrLn $ "parse nakedQuotation: " ++ v3Str
  putStrLn $ show v3 ++ "\n"




