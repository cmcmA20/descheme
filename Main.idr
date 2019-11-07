module Main

import LispCore
import LispPrint
import LispParse
import LispEval

%default total

namespace Main

  replBody : () -> String -> Maybe (String, ())
  replBody () str =
    case str of
         "quit" => Nothing
         ""     => Just ("", ())
         _      => case runParseExpr str of
                        Left  err => Just ("Error: " ++ err ++ "\n", ())
                        Right res => Just (show (eval res) ++ "\n" , ())

  main : IO ()
  main = assert_total $ do
    replWith () "descheme> " replBody
