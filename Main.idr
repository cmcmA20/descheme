module Main

import LispCore
import LispParser
import LispEval

%default total

namespace Main

  replBody : String -> String
  replBody str =
    case runParseExpr str of
         Left  err => "Error: " ++ err ++ "\n"
         Right res => show (eval res) ++ "\n"

  main : IO ()
  main = assert_total $ do
    repl "descheme> " replBody
