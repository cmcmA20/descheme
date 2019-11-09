module Scheme.Print

import Data.Vect
import Scheme.Core

%hide Data.Vect.(::)

%default total
%access public export

showSequence : Show a => List a -> String
showSequence = concat . intersperse " " . map show

showDotted : Show a => Maybe a -> String
showDotted Nothing  = ""
showDotted (Just x) = " . " ++ show x

Show SNum where
  show (SNExactInt n) = show n

Show SExp where
  -- Short special forms
  show (SECons (SESymbol "quote"           ) x) = "'"  ++ show x
  show (SECons (SESymbol "quasiquote"      ) x) = "`"  ++ show x
  show (SECons (SESymbol "unquote"         ) x) = ","  ++ show x
  show (SECons (SESymbol "unquote-splicing") x) = ",@" ++ show x
  show (SECons (SESymbol "lambda"          ) x) = "λ"  ++ show x

  -- Lists and dotted lists
  show (SECons x y) =
    assert_total $
    case listify $ SECons x y of
         (ss, md) => "(" ++ showSequence ss ++ showDotted md ++ ")"

  -- terminals
  show  SEUnit           = "()"
  show (SEVect    xs   ) = "#(" ++ assert_total (showSequence (toList xs)) ++ ")"
  show (SESymbol  t    ) = t
  show (SENum     sn   ) = show sn
  show (SEStr     s    ) = show s
  show (SEBool    False) = "#f"
  show (SEBool    True ) = "#t"
  show (SEChar    c    ) = "#\\" ++ case c of
                                         ' '  => "space"
                                         '\t' => "tab"
                                         '\n' => "newline"
                                         _    => pack [c]

