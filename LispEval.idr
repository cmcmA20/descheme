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

  isBoolean : List (LispV) -> LispV
  isBoolean ((LVBool _) :: []) = LVBool True
  isBoolean _ = LVBool False

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
  stringToSymbol _ = LVStr "undefined"

  classOf : List (LispV) -> LispV
  classOf ((LVAtom       _) :: []) = LVStr "Atom"
  classOf ((LVList    _   ) :: []) = LVStr "List"
  classOf ((LVDotList _  _) :: []) = LVStr "Dotted List"
  classOf ((LVInt        _) :: []) = LVStr "Number"
  classOf ((LVStr        _) :: []) = LVStr "String"
  classOf ((LVBool       _) :: []) = LVStr "Boolean"
  classOf ((LVChar       _) :: []) = LVStr "Char"
  classOf _ = LVStr "undefined"

  primitives : List ((String, List (LispV) -> LispV))
  primitives = assert_total $
    [ ("+", numericBinOp (+))
    , ("-", numericBinOp (-))
    , ("*", numericBinOp (*))
    , ("mod", numericBinOp mod)
    , ("quotient", numericBinOp div)
    , ("remainder", numericBinOp mod)

    , ("bool"   , isBoolean)
    , ("symbol?", isSymbol )
    , ("string?", isString )
    , ("number?", isNumber )
    , ("char?"  , isChar   )

    , ("symbol->string", symbolToString)
    , ("string->symbol", stringToSymbol)

    , ("class-of", classOf)
    ]

  apply : String -> List (LispV) -> LispV
  apply func args = maybe (LVStr "undefined") (\u => u args) $ lookup func primitives

  eval : LispV -> LispV
  eval (LVList ((LVAtom "quote") :: val :: [])) = val
  eval (LVList ((LVAtom func   ) :: args     )) = apply func $ assert_total $ map eval args
  eval val = val

