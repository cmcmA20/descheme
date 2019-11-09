module Scheme.Eval

import Data.Vect
import Data.SortedMap
import Control.ST

import Scheme.Core

%default total
%access public export

SErr : Type
SErr = String

SEvalResult : Type
SEvalResult = Either SErr SExp

data Arity : Type where
  AConst : Arity
  AFixed : Nat -> Arity
  AVar   : Arity

Show Arity where
  show AConst     = "AConst"
  show (AFixed n) = "AFixed " ++ show n
  show AVar       = "AVar"

data SFun : Arity -> Type where
  MkSFConst : SExp -> SFun AConst
  MkSFFixed : (Vect n SExp -> SEvalResult) -> SFun (AFixed n)
  MkSFVar   : (List SExp -> SEvalResult) -> SFun AVar

Show SExp => Show (SFun ar) where
  show      (MkSFConst x) = show x
  show {ar} _             = "#<function " ++ show ar

record SEnv where
  constructor MkSEnv
  definedScope : SortedMap String (ar ** SFun ar)

apply : String -> List SExp -> ST m SEvalResult [env ::: State SEnv]
apply {env} t argList = do
  MkSEnv defs <- read env
  case lookup t defs of
       Nothing                           => pure $ Left "Undefined function"
       Just (AConst   ** MkSFConst _   ) => pure $ Left $ "Can't apply constant " ++ t
       Just (AFixed n ** MkSFFixed func) => do
         case decEq n (length argList) of
              Yes prf => pure $ func $ replace {P = \u => Vect u SExp} (sym prf) $ fromList argList
              No  _   => pure $ Left $ "Need " ++ show n ++ " argument(s), got " ++ show (length argList)
       Just (AVar     ** MkSFVar   func) => pure $ func argList

eval : SExp -> ST m SEvalResult [env ::: State SEnv, qql ::: State Nat]

-- Terminals (Unit, Vect, Bool, Char, Str, Num)
eval SEUnit      = pure $ Right $ SEUnit
eval (SEVect xs) = pure $ Right $ SEVect xs
eval (SEBool b ) = pure $ Right $ SEBool b
eval (SEChar c ) = pure $ Right $ SEChar c
eval (SEStr  s ) = pure $ Right $ SEStr s
eval (SENum sn ) = pure $ Right $ SENum sn

-- Symbols
eval {env} {qql} (SESymbol t) = do
  n <- read qql
  case n of
       -- Outside a quasiquote symbols should consult with environment
       Z   => do MkSEnv defs <- read env
                 case lookup t defs of
                      Nothing                         => pure $ Left $ "Unbound symbol " ++ t
                      Just (AConst ** MkSFConst body) => pure $ Right body
                      Just (ar     ** _             ) => pure $ Left "God bless you" -- FIXME consider arity
       -- Inside a quasiquote symbols evaluate to themselves
       S _ => pure $ Right $ SESymbol t

-- Special forms
eval {env} (SECons (SESymbol "define") (SECons (SESymbol name) (SECons x SEUnit))) = do
  MkSEnv defs <- read env
  case !(eval x) of
       Left  err => pure $ Left err
       Right res => do
         write env $ MkSEnv $ insert name (AConst ** MkSFConst res) defs
         pure $ Right $ SEUnit

eval {env} (SECons (SESymbol "define") (SECons (SECons (SESymbol name) args) (SECons x SEUnit))) = do
  MkSEnv defs <- read env
  case !(eval x) of
       Left  err => pure $ Left err
       Right res => do
         ?definingFunctions

eval (SECons (SESymbol "quote") x) = pure $ Right x

eval {qql} (SECons (SESymbol "quasiquote") x) = do
  n <- read qql
  update qql S
  res <- eval x
  write qql n
  pure res

eval {qql} (SECons (SESymbol "unquote") x) = do
  n <- read qql
  case n of
       Z   => pure $ Left "Unquote only works inside a quasiquote"
       S k => do write qql k
                 res <- eval x
                 write qql n
                 pure res

-- Generic cons i.e. function application
eval {env} {qql} (SECons x y) = do
  n <- read qql
  case n of
       -- Outside a quasiquote
       Z   => do
         case x of
              SESymbol f => call $ apply f $ fst $ listify y
              _          => pure $ Left "Not a function"
       -- Inside a quasiquote evaluate to itself deeply
       S k => do
         exr <- eval x
         eyr <- eval y
         case (exr, eyr) of
              (Left xErr, _        ) => pure $ Left xErr
              (_        , Left yErr) => pure $ Left yErr
              (Right xr , Right yr ) => pure $ Right $ SECons xr yr

-- Everything else doesn't exist yet
eval _ = pure $ Left "Not yet implemented"

namespace StdLib

  -- Type predicates
  isSymbol : Vect 1 SExp -> SExp
  isSymbol [SESymbol _] = SEBool True
  isSymbol _            = SEBool False

  isList : Vect 1 SExp -> SExp
  isList [x] = case snd $ listify x of
                    Nothing => SEBool True
                    _       => SEBool False

  isVect : Vect 1 SExp -> SExp
  isVect [SEVect _] = SEBool True
  isVect _          = SEBool False

  isBool : Vect 1 SExp -> SExp
  isBool [SEBool _] = SEBool True
  isBool _          = SEBool False

  isChar : Vect 1 SExp -> SExp
  isChar [SEChar _] = SEBool True
  isChar _          = SEBool False

  isStr : Vect 1 SExp -> SExp
  isStr [SEStr _] = SEBool True
  isStr _         = SEBool False

  isNum : Vect 1 SExp -> SExp
  isNum [SENum _] = SEBool True
  isNum _         = SEBool False

  -- String/Symbol conversion
  symbolToString : Vect 1 SExp -> SEvalResult
  symbolToString [SESymbol t] = Right $ SEStr t
  symbolToString _            = Left $ "Expected a symbol"

  stringToSymbol : Vect 1 SExp -> SEvalResult
  stringToSymbol [SEStr s] = Right $ SESymbol s
  stringToSymbol _         = Left $ "Expected a string"

  -- Numeric
  plusBin' : SExp -> SExp -> SEvalResult
  plusBin' (SENum (SNExactInt x)) (SENum (SNExactInt y)) = Right $ SENum $ SNExactInt $ x + y
  plusBin' _ _ = Left $ "Expected only numbers"

  plusVar : List SExp -> SEvalResult
  plusVar [] = Right $ SENum $ SNExactInt 0
  plusVar (x :: xs) = do
    w <- plusVar xs
    plusBin' x w

  defaultEnv : SEnv
  defaultEnv = MkSEnv $
    insert "symbol?" (AFixed 1 ** MkSFFixed (pure . isSymbol)) $
    insert "list?" (AFixed 1 ** MkSFFixed (pure . isList)) $
    insert "vector?" (AFixed 1 ** MkSFFixed (pure . isVect)) $
    insert "bool?" (AFixed 1 ** MkSFFixed (pure . isBool)) $
    insert "char?" (AFixed 1 ** MkSFFixed (pure . isChar)) $
    insert "string?" (AFixed 1 ** MkSFFixed (pure . isStr)) $
    insert "number?" (AFixed 1 ** MkSFFixed (pure . isNum)) $
    insert "symbol->string" (AFixed 1 ** MkSFFixed symbolToString) $
    insert "string->symbol" (AFixed 1 ** MkSFFixed stringToSymbol) $
    insert "+" (AVar ** MkSFVar plusVar) $
    empty

