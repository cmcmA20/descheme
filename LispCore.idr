module LispCore

import Data.String

%default total
%access public export

namespace LispType

  data LispV : Type where
    LVAtom    : String -> LispV
    LVList    : List (LispV) -> LispV
    LVDotList : List (LispV) -> LispV -> LispV
    LVInt     : Integer -> LispV
    LVStr     : String -> LispV
    LVBool    : Bool -> LispV
    LVChar    : Char -> LispV


namespace LispPrint

  showSequence : Show a => List a -> String
  showSequence = concat . intersperse " " . map show

  Show LispV where
    show (LVAtom       t) = t
    show (LVList    xs  ) = "(" ++ assert_total (showSequence xs) ++ ")"
    show (LVDotList xs x) = "(" ++ assert_total (showSequence xs) ++ ". " ++ show x ++ ")"
    show (LVInt        n) = show n
    show (LVStr        s) = show s
    show (LVBool   False) = "#f"
    show (LVBool   True ) = "#t"
    show (LVChar       c) =
      "#\\" ++ case c of
                    ' '  => "space"
                    '\t' => "tab"
                    '\n' => "newline"
                    _    => pack [c]

