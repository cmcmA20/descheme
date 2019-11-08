module LispCore

import Data.Vect

%default total
%access public export

namespace LispCore

  mutual
    ||| [L]isp [Value]
    data LValue : Type where
      ||| Symbols are just identifiers or pointers
      ||| They evaluate to something if defined in current scope
      LVSymbol  : String -> LValue
      ||| List is the basic building block and the only way to apply a function
      LVList    : List (LValue) -> LValue
      ||| DotList is a pair of a list and a single value
      ||| Usual lists are DotLists where the single value is an empty list
      LVDotList : List (LValue) -> LValue -> LValue
      ||| Numbers are terminal
      LVNum     : LNum -> LValue
      ||| Strings are terminal
      LVStr     : String -> LValue
      ||| Booleans are terminal
      LVBool    : Bool -> LValue
      ||| Chars are terminal
      LVChar    : Char -> LValue
      ||| Vectors are terminal
      LVVect    : Vect n LValue -> LValue

    -- TODO
    -- Integer <= Rational <= Real <= Complex
    -- Exact | Inexact
    ||| [L]isp [Num]ber
    data LNum : Type where
      LNExactInt : Integer -> LNum

