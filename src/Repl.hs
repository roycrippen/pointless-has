module Repl where

import           Control.Monad (forever)
import           Core          (coreDefinitions)
import qualified Data.Map      as M (fromList)
import           Interpreter   (Lang (..), Mode (..), replaceStr)
import           Primitives    (runQuotStr)
import           System.Exit   (exitSuccess)
import           System.IO     (hFlush, stdout)

startRepl :: IO ()
startRepl = do
  putStrLn "Welcome to the Pointless repl    (:h for help)"
  putStrLn "Pointless> "
  let defs = coreDefinitions
      lang = Lang (M.fromList defs) [] [] "" REPL
  runPointless lang

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
        ""   -> runPointless lang
        _    -> do
          let lang' = runQuotStr s lang
          mapM_ putStrLn (result lang')
          runPointless lang' { result = [] }

showHelp :: IO ()
showHelp = do
  putStrLn ":h                  -> show help"
  putStrLn ":q                  -> exit"
  putStrLn "Ctrl+c              -> exit"
  putStrLn "<filename> libload  -> load external Pointless source file"
  putStrLn "<expression>        -> for example enter: 1 2 + ."
  putStrLn "Pointless> "

