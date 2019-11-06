module LispEval

import LispCore

%default total
%access public export

namespace LispEval

  unpackNum : LispV -> Integer
  unpackNum (LVInt n) = n
  unpackNum _ = 0

  numericBinOp : (Integer -> Integer -> Integer) -> List (LispV) -> LispV
  numericBinOp op params = assert_total $ LVInt $ foldl1 op $ map unpackNum params

  isSymbol : List (LispV) -> LispV
  isSymbol ((LVAtom _) :: []) = LVBool True
  isSymbol _ = LVBool False

  isString : List (LispV) -> LispV
  isString ((LVStr _) :: []) = LVBool True
  isString _ = LVBool False

  isNumber : List (LispV) -> LispV
  isNumber ((LVInt _) :: []) = LVBool True
  isNumber _ = LVBool False

  isChar : List (LispV) -> LispV
  isChar ((LVChar _) :: []) = LVBool True
  isChar _ = LVBool False

  symbolToString : List (LispV) -> LispV
  symbolToString ((LVAtom t) :: []) = LVStr t
  symbolToString _ = LVStr ""

  stringToSymbol : List (LispV) -> LispV
  stringToSymbol ((LVStr s) :: []) = LVAtom s
  stringToSymbol _ = LVAtom "undefined"

  primitives : List ((String, List (LispV) -> LispV))
  primitives = assert_total $
    [ ("+", numericBinOp (+))
    , ("-", numericBinOp (-))
    , ("*", numericBinOp (*))
    , ("mod", numericBinOp mod)
    , ("quotient", numericBinOp div)
    , ("remainder", numericBinOp mod)

    , ("symbol?", isSymbol)
    , ("string?", isString)
    , ("number?", isNumber)
    , ("char?"  , isChar  )

    , ("symbol->string", symbolToString)
    , ("string->symbol", stringToSymbol)
    ]

  apply : String -> List (LispV) -> LispV
  apply func args = maybe (LVBool False) (\u => u args) $ lookup func primitives

  eval : LispV -> LispV
  eval (LVList ((LVAtom "quote") :: val :: [])) = val
  eval (LVList ((LVAtom func   ) :: args     )) = apply func $ assert_total $ map eval args
  eval val = val

