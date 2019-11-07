module LispPrint

import LispCore

%default total
%access public export

namespace LispPrint

  showSequence : Show a => List a -> String
  showSequence = concat . intersperse " " . map show

  Show LNum where
    show (LNExact n) = show n

  Show LValue where
    show (LVList (LVSymbol "quote"      :: [x])  ) = "'" ++ show x
    show (LVList (LVSymbol "quasiquote" :: [x])  ) = "`" ++ show x
    show (LVList (LVSymbol "unquote"    :: [x])  ) = "," ++ show x

    show (LVSymbol  t    ) = t
    show (LVList    xs   ) = "(" ++ assert_total (showSequence xs) ++ ")"
    show (LVDotList xs x ) = "(" ++ assert_total (showSequence xs) ++ ". " ++ show x ++ ")"
    show (LVNum     ln   ) = show ln
    show (LVStr     s    ) = show s
    show (LVBool    False) = "#f"
    show (LVBool    True ) = "#t"
    show (LVChar    c    ) =
      "#\\" ++ case c of
                    ' '  => "space"
                    '\t' => "tab"
                    '\n' => "newline"
                    _    => pack [c]

