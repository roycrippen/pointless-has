module Repl where

import           CLaSH.Prelude       hiding (many, (++), (<|>))
import           Control.Applicative (Applicative (..), pure)
import           Control.Monad       (forever)
import           Control.Monad       (Functor (..), Monad (..), ap, liftM, void)
import           Data.Bool
import           Data.Char
import           Data.Eq
import           Data.Function
import           Data.Int
import qualified Data.List           as L
import qualified Data.Map            as M (fromList)
import           Data.Maybe          (isJust)
import           Data.String
import           Interpreter
import           Parser
import qualified Prelude             as P
import           Primitives          (coreDefinitions, primitiveAST, runQuotStr)
import           System.Exit         (exitSuccess)
import           System.IO           (hFlush, stdout)
import           Text.Read

startRepl :: IO ()
startRepl = do
  putStrLn "Welcome to the Pointless repl    (:h for help)"
  putStrLn "Pointless> "
  let lang = Lang coreDefinitions [] [] "" REPL
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
  runCommand s = case L.take 2 s of
    ":q" -> exitSuccess
    ":h" -> showHelp >> runPointless lang
    ""   -> runPointless lang
    _    -> do
      let (qs, _):_ = parse nakedQuotations s
          lang'     = runQuotation qs lang
      mapM_ putStrLn (result lang')
      -- this will infinite loop if pointless function recursively calls its self
      -- mapM_ print $ primitiveAST (vocab lang') qs
      runPointless lang' { result = [] }

showHelp :: IO ()
showHelp = do
  putStrLn ":h                  -> show help"
  putStrLn ":q                  -> exit"
  putStrLn "Ctrl+c              -> exit"
  putStrLn "<filename> libload  -> load external Pointless source file"
  putStrLn "<expression>        -> for example enter: 1 2 + ."
  putStrLn "Pointless> "










