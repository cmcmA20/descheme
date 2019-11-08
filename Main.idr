module Main

import Data.SortedMap
import Data.Fuel
import Control.ST

import LispCore
import LispPrint
import LispParse
import LispEval

%default total

namespace Main

  replStep : String -> (env, qql : Var) -> ST m (Maybe String) [env ::: State LEnv, qql ::: State Nat]
  replStep userInput env qql = do
    case userInput of
         "quit" => pure Nothing
         ""     => pure $ Just ""
         "env"  => do
           MkLEnv defs <- read env
           pure $ Just $ show defs
         _      => case runParseExpr userInput of
                        Left  err => do
                          pure $ Just $ "Parse error!\n" ++ err
                        Right x   => do
                          case !(eval {env} {qql} x) of
                               Left  err => do
                                 pure $ Just $ "Evaluation error!\n" ++ err
                               Right res => do
                                 pure $ Just $ show res

  -- FIXME Tricking the totality checker is bad, mkay
  repl2 : ConsoleIO io => (env, qql : Var) -> ST io () [env ::: State LEnv, qql ::: State Nat]
  repl2 env qql = do
    putStr "descheme> "
    userInput <- getStr
    res <- replStep userInput env qql
    case res of
         Nothing => pure ()
         Just r  => do
           putStrLn r
           assert_total $ repl2 env qql

  createDefaultREPL : ConsoleIO io => ST io () []
  createDefaultREPL = do
    qql <- new $ the Nat 0
    env <- new $ defaultEnv
    repl2 env qql
    delete env
    delete qql
    pure ()

  -- FIXME Control+D ruins it
  main : IO ()
  main = assert_total $ do
    run createDefaultREPL
    pure ()

