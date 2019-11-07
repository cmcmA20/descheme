module LispCore

%default total
%access public export

namespace LispCore

  mutual
    data LValue : Type where
      LVSymbol  : String -> LValue
      LVList    : List (LValue) -> LValue
      LVDotList : List (LValue) -> LValue -> LValue
      LVNum     : LNum -> LValue
      LVStr     : String -> LValue
      LVBool    : Bool -> LValue
      LVChar    : Char -> LValue

    -- TODO
    -- Integer <= Rational <= Real <= Complex
    -- Exact | Inexact
    data LNum : Type where
      LNExact   : Integer -> LNum

