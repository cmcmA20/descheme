module LispEval

import Control.ST
import Data.SortedMap

import LispCore

%default total
%access public export

namespace LispEval

  record LEnv where
    constructor MkLEnv
    definedSymbols : SortedMap String LValue

  -- LEnv : Type
  -- LEnv = SortedMap String LValue

  defaultEnv : LEnv
  defaultEnv = MkLEnv empty

  LErr : Type
  LErr = String

  LEvalResult : Type
  LEvalResult = Either LErr LValue

  unpackNum' : LValue -> ST m LEvalResult []
  unpackNum' (LVNum (LNExactInt n)) = pure $ Right $ LVNum $ LNExactInt n
  unpackNum' _ = pure $ Left "Number required"

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
  classOf ((LVSymbol     _) :: []) = LVStr "Atom"
  classOf ((LVList    _   ) :: []) = LVStr "List"
  classOf ((LVDotList _  _) :: []) = LVStr "Dotted List"
  classOf ((LVNum        _) :: []) = LVStr "Number"
  classOf ((LVStr        _) :: []) = LVStr "String"
  classOf ((LVBool       _) :: []) = LVStr "Boolean"
  classOf ((LVChar       _) :: []) = LVStr "Char"
  classOf _ = LVStr "undefined"

  unpackNum : LValue -> Integer
  unpackNum (LVNum (LNExactInt n)) = n
  unpackNum _ = 0

  numericBinOp : (Integer -> Integer -> Integer) -> List (LValue) -> LValue
  numericBinOp op params = assert_total $ LVNum $ LNExactInt $ foldl1 op $ map unpackNum params

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

  eval : LValue -> ST m LEvalResult [env ::: State LEnv, qql ::: State Nat]
  eval {env} (LVList (LVSymbol "define" :: LVSymbol name :: [x])) = do
    MkLEnv defs <- read env
    case !(eval x) of
         Left  err => pure $ Left err
         Right val => do
           write env $ MkLEnv $ insert name val defs
           pure $ Right $ LVList [] -- FIXME using dependent types to express a fkn placeholder value

  eval (LVList (LVSymbol "quote" :: [x])) = pure $ Right x

  eval {qql} (LVList (LVSymbol "quasiquote" :: [x])) = do
    update qql S
    eval x

  eval {qql} (LVList (LVSymbol "unquote" :: [x])) = do
    n <- read qql
    case n of
         Z   => pure $ Left "Unquote only works inside a quasiquote"
         S k => do
           write qql k
           eval x

  -- TODO should track function arity
  eval {env} (LVList (LVSymbol func :: args)) = do
    ?whattodo

  eval {env} (LVSymbol x) = do
    MkLEnv defs <- read env
    case lookup x defs of
         Nothing   => pure $ Left "Undefined symbol"
         Just body => pure $ Right body

  eval (LVNum  ln) = pure $ Right $ LVNum  ln
  eval (LVChar c ) = pure $ Right $ LVChar c
  eval (LVStr  s ) = pure $ Right $ LVStr  s
  eval (LVBool b ) = pure $ Right $ LVBool b

  eval _ = pure $ Left "Not yet implemented"

  --mutual
    -- eval (LVList (LVSymbol func         :: args     )) = apply func $ assert_total $ map eval args

