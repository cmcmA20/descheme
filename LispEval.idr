module LispEval

import Control.ST
import LispCore

%default total
%access public export

namespace LispEval

  -- Resources to track in eval:
  -- quasiquote level -- Nat
  -- environment -- List of bound symbols

  -- LispEnv : Type
  -- LispEnv = State (List String)

  -- data LErr : Type where
  --   LE : -> LErr

  -- data LEvalResult : Type where
  --   LERFail : LErr -> LEvalResult
  --   LERSuccess : LValue -> LEvalResult

  -- eas : (ql : Var) -> (env : Var) -> LValue -> ST m Integer [ ql ::: State Nat, env ::: LispEnv, err ::: LispErr ]
  -- eas ql env x = ?eas_rhs

  -- evalInt : ST m Integer []
  -- evalInt = ?evalInt_rhs

  unpackNum : LValue -> Integer
  unpackNum (LVNum (LNExact n)) = n
  unpackNum _ = 0

  numericBinOp : (Integer -> Integer -> Integer) -> List (LValue) -> LValue
  numericBinOp op params = assert_total $ LVNum $ LNExact $ foldl1 op $ map unpackNum params

  isBoolean : List (LValue) -> LValue
  isBoolean ((LVBool _) :: []) = LVBool True
  isBoolean _ = LVBool False

  isSymbol : List (LValue) -> LValue
  isSymbol ((LVSymbol _) :: []) = LVBool True
  isSymbol _ = LVBool False

  isString : List (LValue) -> LValue
  isString ((LVStr _) :: []) = LVBool True
  isString _ = LVBool False

  isNumber : List (LValue) -> LValue
  isNumber ((LVNum _) :: []) = LVBool True
  isNumber _ = LVBool False

  isChar : List (LValue) -> LValue
  isChar ((LVChar _) :: []) = LVBool True
  isChar _ = LVBool False

  symbolToString : List (LValue) -> LValue
  symbolToString ((LVSymbol t) :: []) = LVStr t
  symbolToString _ = LVStr ""

  stringToSymbol : List (LValue) -> LValue
  stringToSymbol ((LVStr s) :: []) = LVSymbol s
  stringToSymbol _ = LVStr "undefined"

  classOf : List (LValue) -> LValue
  classOf ((LVSymbol       _) :: []) = LVStr "Atom"
  classOf ((LVList    _   ) :: []) = LVStr "List"
  classOf ((LVDotList _  _) :: []) = LVStr "Dotted List"
  classOf ((LVNum        _) :: []) = LVStr "Number"
  classOf ((LVStr        _) :: []) = LVStr "String"
  classOf ((LVBool       _) :: []) = LVStr "Boolean"
  classOf ((LVChar       _) :: []) = LVStr "Char"
  classOf _ = LVStr "undefined"

  primitives : List ((String, List (LValue) -> LValue))
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

  apply : String -> List (LValue) -> LValue
  apply func args = maybe (LVStr "undefined") (\u => u args) $ lookup func primitives

  mutual
    evalUnquotes : LValue -> LValue
    evalUnquotes (LVList    (LVSymbol "unquote" :: val :: [])) = eval val
    evalUnquotes (LVList    xs                             ) = LVList $ assert_total $ map evalUnquotes xs
    evalUnquotes (LVDotList xs x                           ) = LVDotList (assert_total $ map eval xs) $ eval x
    evalUnquotes val                                         = val

    eval : LValue -> LValue
    eval (LVList (LVSymbol "quote"      :: val :: [])) = val
    eval (LVList (LVSymbol "quasiquote" :: val :: [])) = evalUnquotes val
    eval (LVList (LVSymbol "unquote"    :: val :: [])) = LVStr "ERROR: unquote must be within quasiquote"
    eval (LVList (LVSymbol func         :: args     )) = apply func $ assert_total $ map eval args
    eval val                                         = val

