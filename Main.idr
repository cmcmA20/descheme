module Main

import Parser
import LispParser
import LispEval

%default total

namespace Main

  replBody : String -> String
  replBody str =
    case parse parseExpr str of
         Left  err       => "ERROR\n" ++ err ++ "\n"
         Right (r, rest) => case unpack rest of
                                 [] => show (eval r) ++ "\n"
                                 _  => "ERROR\nUnparsed leftover: \"" ++ rest ++ "\"\n"

  main : IO ()
  main = assert_total $ do
    repl "descheme> " replBody
