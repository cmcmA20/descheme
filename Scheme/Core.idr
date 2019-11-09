module Scheme.Core

import Control.Isomorphism
import Data.Vect

%default total
%access public export

-- TODO Integer <= Rational <= Real <= Complex
-- TODO Exact | Inexact
||| [S]cheme [Num]ber
data SNum : Type where
  SNExactInt : Integer -> SNum

||| [S]ymbolic [Exp]ression
data SExp : Type where
  ||| Symbols are just identifiers or pointers
  ||| They evaluate to something if defined in current scope
  ||| Some are special forms that are evaluated under different rules
  SESymbol  : String -> SExp
  ||| Unit is an empty list
  SEUnit    : SExp
  ||| Cons is the basic building block and the only way to apply a function
  SECons    : SExp -> SExp -> SExp
  ||| Vector should be just an O(1) access List
  ||| Vectors are terminal
  SEVect    : Vect n SExp -> SExp
  ||| Booleans are terminal
  SEBool    : Bool -> SExp
  ||| Chars are terminal
  SEChar    : Char -> SExp
  ||| Strings are terminal
  SEStr     : String -> SExp
  ||| Numbers are terminal
  SENum     : SNum -> SExp

listify : SExp -> (List SExp, Maybe SExp)
listify SEUnit       = ([], Nothing)
listify (SECons x y) = let (ls, md) = listify y
                        in (x :: ls, md)
listify x            = ([], Just x)

unListify : (List SExp, Maybe SExp) -> SExp
unListify (xs, ms) = foldr SECons (maybe SEUnit id ms) xs

-- unListify : (List SExp, Maybe SExp) -> SExp
-- unListify ([]     , Nothing) = SEUnit
-- unListify ([]     , Just s ) = s
-- unListify (x :: xs, ms     ) = SECons x $ assert_total $ unListify (xs, ms)

-- ||| A theorem about lists and dotted lists
-- lispIso : Iso SExp (List SExp, Maybe SExp)
-- lispIso = MkIso to from toFrom fromTo
--   where
--     to : SExp -> (List SExp, Maybe SExp)
--     to = listify
--     from : (List SExp, Maybe SExp) -> SExp
--     from = unListify
--     toFrom : (p : (List SExp, Maybe SExp)) -> to (from p) = p
--     toFrom ([]     , Nothing) = Refl
--     toFrom ([]     , Just s ) = ?toFrom_rhs_5
--     toFrom (x :: xs, Nothing) = ?toFrom_rhs_4
--     toFrom (x :: xs, Just s ) = ?toFrom_rhs_1
--     fromTo : (s : SExp) -> from (to s) = s
--     fromTo SEUnit       = Refl
--     fromTo (SESymbol x) = Refl
--     fromTo (SEVect xs ) = Refl
--     fromTo (SEBool x  ) = Refl
--     fromTo (SEChar x  ) = Refl
--     fromTo (SEStr x   ) = Refl
--     fromTo (SENum x   ) = Refl
--     fromTo (SECons x y) = ?whasdasd

