module LispParser

import Data.String
import Parser

%default total
%access public export

namespace LispParser

  data LispV : Type where
    LVAtom    : String -> LispV
    LVList    : List (LispV) -> LispV
    LVDotList : List (LispV) -> LispV -> LispV
    LVInt     : Integer -> LispV
    LVStr     : String -> LispV
    LVBool    : Bool -> LispV
    LVChar    : Char -> LispV

  showSequence : Show a => List a -> String
  showSequence = concat . intersperse " " . map show

  Show LispV where
    show (LVAtom       t) = t
    show (LVList    xs  ) = "(" ++ assert_total (showSequence xs) ++ ")"
    show (LVDotList xs x) = "(" ++ assert_total (showSequence xs) ++ ". " ++ show x ++ ")"
    show (LVInt        n) = show n
    show (LVStr        s) = "\\" ++ show s ++ "\\"
    show (LVBool   False) = "#f"
    show (LVBool   True ) = "#t"
    show (LVChar       c) = "#\\" ++ case c of
                                     ' '  => "space"
                                     '\t' => "tab"
                                     '\n' => "newline"
                                     _    => pack [c]

  backslashOrDoubleQuote : Parser (NonEmptyList Char)
  backslashOrDoubleQuote = do
    x <- char '\\' *> (oneOf $ unpack "\\\"")
    pure $ MkNEList ([x] ** IsNonEmpty)

  parseChar : Parser LispV
  parseChar = do
    char '#'
    char '\\'
    name <- many1 (letter <|> symbol <|> parens)
    case name of
         (MkNEList (c :: [] ** _)) => pure $ LVChar c
         (MkNEList (c :: cs ** _)) => case toLower (pack (c :: cs)) of
                                           "space"   => pure $ LVChar ' '
                                           "tab"     => pure $ LVChar '\t'
                                           "newline" => pure $ LVChar '\n'
                                           _         => never "Can't parse character"

  parseStr : Parser LispV
  parseStr = do
    char '"'
    xs <- many $ backslashOrDoubleQuote <|> many1 (noneOf (unpack "\\\""))
    char '"'
    pure $ LVStr $ pack $ concat $ map neWeaken xs

  -- improper cases should be eliminated by restricting the type of parser
  parseBool : Parser LispV
  parseBool = do
    char '#'
    x <- oneOf $ unpack "tf"
    pure $ case x of
                't' => LVBool True
                _   => LVBool False

  parseAtom : Parser LispV
  parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = pack $ first :: rest
    pure $ case atom of
           "#t" => LVBool True
           "#f" => LVBool False
           _    => LVAtom atom

  parseInt : Parser LispV
  parseInt = do
    sign <- oneOf (unpack "+-") <|> pure 'v'
    xs <- parsePositive . pack . neWeaken <$> many1 digit
    case xs of
         Nothing => never "Can't parse number"
         Just n  => pure $ LVInt $ if sign == '-'
                                      then -n
                                      else  n

  mutual
    parseExpr : Parser LispV
    parseExpr = assert_total $ parseInt <|> parseChar <|> parseAtom <|> parseStr <|> parseBool <|> parseQuoted <|> parseAnyList

    parseQuoted : Parser LispV
    parseQuoted = do
      char '\''
      x <- parseExpr
      pure $ LVList [LVAtom "quote", x]

    parseMaybeDotExpr : Parser (Maybe LispV)
    parseMaybeDotExpr =
      Just <$> (char '.' *> spaces *> parseExpr) <|>
      pure Nothing

    parseAnyList : Parser LispV
    parseAnyList = do
      char '('
      xs <- sepBy parseExpr spaces
      mx <- parseMaybeDotExpr
      char ')'
      pure $ case mx of
                  Nothing => LVList xs
                  Just x  => LVDotList xs x

