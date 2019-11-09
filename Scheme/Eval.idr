module Scheme.Eval

import Data.Vect
import Data.SortedMap
import Control.ST

import Scheme.Core

%default total
%access public export

-- symbol can be either constant
-- or forall n : Nat of fixed arity n
-- or of variable arity

-- TODO Add functions of n arguments
record SEnv where
  constructor MkSEnv
  definedSymbols : SortedMap String SExp

defaultEnv : SEnv
defaultEnv = MkSEnv empty

SErr : Type
SErr = String

SEvalResult : Type
SEvalResult = Either SErr SExp

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
       -- Not in a quasiquotation, so need to evaluate symbols
       Z   => do MkSEnv defs <- read env
                 case lookup t defs of
                      Nothing   => pure $ Left "Undefined symbol"
                      Just body => pure $ Right body
       -- Inside a quasiquotation, symbols evaluate to themselves
       S _ => pure $ Right $ SESymbol t

-- Special forms
eval {env} (SECons (SESymbol "define") (SECons (SESymbol name) (SECons x SEUnit))) = do
  MkSEnv defs <- read env
  case !(eval x) of
       Left  err => pure $ Left err
       Right res => do
         write env $ MkSEnv $ insert name res defs
         pure $ Right $ SEUnit

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
-- eval (SECons x y) = ?evalConsApply -- TODO

-- Everything else doesn't exist yet
eval _ = pure $ Left "Not yet implemented"

-- unpackNum' : SExp -> ST m LEvalResult []
-- unpackNum' (SENum (LNExactInt n)) = pure $ Right $ SENum $ LNExactInt n
-- unpackNum' _ = pure $ Left "Number required"

-- isBoolean : List (SExp) -> SExp
-- isBoolean ((SEBool _) :: []) = SEBool True
-- isBoolean _ = SEBool False

-- isSymbol : List (SExp) -> SExp
-- isSymbol ((SESymbol _) :: []) = SEBool True
-- isSymbol _ = SEBool False

-- isString : List (SExp) -> SExp
-- isString ((SEStr _) :: []) = SEBool True
-- isString _ = SEBool False

-- isNumber : List (SExp) -> SExp
-- isNumber ((SENum _) :: []) = SEBool True
-- isNumber _ = SEBool False

-- isChar : List (SExp) -> SExp
-- isChar ((SEChar _) :: []) = SEBool True
-- isChar _ = SEBool False

-- symbolToString : List (SExp) -> SExp
-- symbolToString ((SESymbol t) :: []) = SEStr t
-- symbolToString _ = SEStr ""

-- stringToSymbol : List (SExp) -> SExp
-- stringToSymbol ((SEStr s) :: []) = SESymbol s
-- stringToSymbol _ = SEStr "undefined"

-- classOf : List (SExp) -> SExp
-- classOf ((SESymbol     _) :: []) = SEStr "Atom"
-- classOf ((LVList    _   ) :: []) = SEStr "List"
-- classOf ((LVDotList _  _) :: []) = SEStr "Dotted List"
-- classOf ((SENum        _) :: []) = SEStr "Number"
-- classOf ((SEStr        _) :: []) = SEStr "String"
-- classOf ((SEBool       _) :: []) = SEStr "Boolean"
-- classOf ((SEChar       _) :: []) = SEStr "Char"
-- classOf _ = SEStr "undefined"

-- unpackNum : SExp -> Integer
-- unpackNum (SENum (LNExactInt n)) = n
-- unpackNum _ = 0

-- numericBinOp : (Integer -> Integer -> Integer) -> List (SExp) -> SExp
-- numericBinOp op params = assert_total $ SENum $ LNExactInt $ foldl1 op $ map unpackNum params

-- primitives : List ((String, List (SExp) -> SExp))
-- primitives = assert_total $
--   [ ("+", numericBinOp (+))
--   , ("-", numericBinOp (-))
--   , ("*", numericBinOp (*))
--   , ("mod", numericBinOp mod)
--   , ("quotient", numericBinOp div)
--   , ("remainder", numericBinOp mod)

--   , ("bool"   , isBoolean)
--   , ("symbol?", isSymbol )
--   , ("string?", isString )
--   , ("number?", isNumber )
--   , ("char?"  , isChar   )

--   , ("symbol->string", symbolToString)
--   , ("string->symbol", stringToSymbol)

--   , ("class-of", classOf)
--   ]

-- apply : String -> List (SExp) -> SExp
-- apply func args = maybe (SEStr "undefined") (\u => u args) $ lookup func primitives

-- TODO should track function arity
-- eval {env} {qql} (LVList (SESymbol func :: args)) = do
--   n <- read qql
--   case n of
--        Z   => ?wsad1
--        S k => pure $ Right $ LVList $ SESymbol func :: args

-- eval all arguments
-- eval {qql} (LVList []       ) = pure $ Right $ LVList []
-- eval {qql} (LVList (x :: xs)) = do
--   n <- read qql
--   case n of
--        Z   => pure $ Left "Not a function"
--        S k => do rs <- evalIndependent $ assert_total $ map eval $ x :: xs
--                  pure $ Right $ LVList rs

