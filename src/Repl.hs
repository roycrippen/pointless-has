module Repl where
-- (startRepl) where

import           Control.Monad   (forever)
import           Core            (coreDefinitions)
import qualified Data.Map        as M (fromList)
import           Interpreter     (Lang (..), Mode (..), runQuotation)
import           Parser          (parse)
import           PointlessParser (nakedQuotations)
import           System.Exit     (exitSuccess)
import           System.IO       (hFlush, stdout)
import           System.IO.Error (tryIOError)

-- | wip, supports combined ouput only
startRepl :: IO ()
startRepl = do
  putStrLn "Welcome to the Pointless repl    (:h for help)"
  putStrLn "Pointless> "
  let defs = coreDefinitions
      lang = Lang (M.fromList defs) [] [] "" REPL
  runPointless lang

runQuot :: String -> Lang -> Lang
runQuot s = runQuotation qs
  where (qs, _):_ = parse nakedQuotations s

runPointless :: Lang -> IO ()
runPointless lang = forever $ do
  putStr "Pointless> "
  hFlush stdout
  quoteStr' <- getLine
  let quoteStr = replaceStr "\\n" "\n" quoteStr'
  runCommand quoteStr
    where
      runCommand :: String -> IO ()
      runCommand s =
        case take 2 s of
        ":q" -> exitSuccess
        ":h" -> showHelp >> runPointless lang
        ":l" -> do
          lang' <- loadAndRunFile s lang
          runPointless lang' { result = [] }
        ":r" -> showHelp >> runPointless lang
        ""   -> runPointless lang
        _    -> do
          let lang' = runQuot s lang
          mapM_ putStrLn (result lang')
          runPointless lang' { result = [] }

loadAndRunFile :: String -> Lang -> IO Lang
loadAndRunFile file lang = do
  let file' = replaceStr ":l " "" file ++ ".pless"
  strOrExc <- tryIOError $ readFile file'
  case strOrExc of
    Left except -> do
      print except
      return lang
    Right source' -> do
      let source = replaceStr  "\\n" "\n" source'
          lang' = runQuot source lang
      mapM_ putStrLn (result lang')
      return lang'

showHelp :: IO ()
showHelp = do
  putStrLn ":h             show help"
  putStrLn ":q             exit"
  putStrLn "Ctrl+c         exit"
  putStrLn ":l <filename>  load a Pointless script"
  putStrLn ":r             reload Pointless script"
  putStrLn "or enter a valid Pointless expression"
  putStrLn "Pointless> "

replaceStr :: String -> String -> String -> String
replaceStr _ _ [] = []
replaceStr old new str = go str
  where
    go [] = []
    go str'@(x:xs) =
      let (prefix, rest) = splitAt n str'
      in
        if old == prefix
          then new ++ go rest
          else x : go xs
    n = length old
