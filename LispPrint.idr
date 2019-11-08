module LispPrint

import Data.Vect
import LispCore

%hide Data.Vect.(::)

%default total
%access public export

namespace LispPrint

  Show LNum where
    show (LNExactInt n) = show n

  showSequence : Show a => List a -> String
  showSequence = concat . intersperse " " . map show

  Show LValue where
    show (LVList (LVSymbol "quote"            :: [x])) = "'"  ++ show x
    show (LVList (LVSymbol "quasiquote"       :: [x])) = "`"  ++ show x
    show (LVList (LVSymbol "unquote"          :: [x])) = ","  ++ show x
    show (LVList (LVSymbol "unquote-splicing" :: [x])) = ",@" ++ show x
    show (LVList (LVSymbol "lambda"           :: [x])) = "λ"  ++ show x

    show (LVVect    xs   ) = "#(" ++ assert_total (showSequence (toList xs)) ++ ")"
    show (LVList    xs   ) = "("  ++ assert_total (showSequence xs) ++ ")"
    show (LVDotList xs x ) = "("  ++ assert_total (showSequence xs) ++ ". " ++ show x ++ ")"

    show (LVSymbol  t    ) = t
    show (LVNum     ln   ) = show ln
    show (LVStr     s    ) = show s
    show (LVBool    False) = "#f"
    show (LVBool    True ) = "#t"
    show (LVChar    c    ) = "#\\" ++ case c of
                                           ' '  => "space"
                                           '\t' => "tab"
                                           '\n' => "newline"
                                           _    => pack [c]

