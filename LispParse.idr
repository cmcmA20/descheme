module LispParse

import Data.List
import Data.Vect
import Data.String

import Parser
import LispCore

%hide Data.Vect.Here
%hide Data.Vect.There

%default total
%access public export

namespace LispParse

  spaces : Parser ()
  spaces = skipSome $ char ' '

  special : Parser Char
  special = oneOf $ unpack "!&|*+-/:<=?>@^_~"

  escapedBackslashOrDoubleQuote : Parser (NonEmptyList Char)
  escapedBackslashOrDoubleQuote = do
    char '\\'
    x <- oneOf $ unpack "\\\""
    pure $ MkNEList ([x] ** IsNonEmpty)

  parseChar : Parser LValue
  parseChar = do
    char '#'
    char '\\'
    name <- exactly (oneOf (unpack "#\\\"'[]{}()") <|> special <|> digit) <|> some letter
    case name of
         (MkNEList (c :: [] ** _)) => pure $ LVChar c
         (MkNEList (c :: cs ** _)) => case toLower (pack (c :: cs)) of
                                           "space"   => pure $ LVChar ' '
                                           "tab"     => pure $ LVChar '\t'
                                           "newline" => pure $ LVChar '\n'
                                           _         => noParse "Can't parse character"

  parseStr : Parser LValue
  parseStr = do
    char '"'
    xs <- many $ escapedBackslashOrDoubleQuote <|> (some $ noneOf $ unpack "\\\"")
    char '"'
    pure $ LVStr $ pack $ concat $ map neWeaken xs

  parseBool : Parser LValue
  parseBool = do
    char '#'
    MkOO (_ ** prf) <- oneOf' $ unpack "tf"
    pure $ case prf of
                Here         => LVBool True
                There Here   => LVBool False
                There (There _) impossible

  parseAtom : Parser LValue
  parseAtom =
    [| (LVSymbol . pack)
       [| (letter <|> special) :: (many (letter <|> digit <|> special)) |]
    |]

  -- TODO exact/inexact, base modifier, numeric hierarchy
  parseNum : Parser LValue
  parseNum = do
    msign <- optional $ oneOf' $ unpack "+-"
    xs    <- parsePositive . pack . neWeaken <$> some digit
    case xs of
         Nothing => noParse "Can't parse number"
         Just n  => pure $ LVNum $ LNExactInt $ case msign of
                                                     Nothing                => n
                                                     Just (MkOO (_ ** prf)) => case prf of
                                                                                    Here         =>  n
                                                                                    There Here   => -n
                                                                                    There (There _) impossible

  mutual
    parseExpr : Parser LValue
    parseExpr =   assert_total
              $   parseNum
              <|> parseStr
              <|> parseAtom
              <|> parseBool
              <|> parseChar
              <|> parseShortForm "'"  "quote"
              <|> parseShortForm "`"  "quasiquote"
              <|> parseShortForm ",@" "unquote-splicing"
              <|> parseShortForm ","  "unquote"
              <|> parseShortForm "λ"  "lambda"
              <|> parseListLike

    parseShortForm : String -> String -> Parser LValue
    parseShortForm short long = do
      string $ unpack short
      x <- parseExpr
      pure $ LVList [LVSymbol long, x]

    parseListLike : Parser LValue
    parseListLike = do
      mv <- optional $ char '#'
      char '('
      xs <- sepBy parseExpr spaces
      mx <- optional $ char '.' *> spaces *> parseExpr
      char ')'
      case (mx, mv) of
           (Just _ , Just _ ) => noParse "Improper vector"
           (Just x , Nothing) => pure $ LVDotList xs x
           (Nothing, Just _ ) => pure $ LVVect $ fromList xs
           (Nothing, Nothing) => pure $ LVList xs

  runParseExpr : String -> Either String LValue
  runParseExpr inp =
    case parse parseExpr inp of
         Left  err       => Left err
         Right (r, rest) => case unpack rest of
                                 []        => Right r
                                 (x :: xs) => Left $ "Trailing garbage: " ++ show rest

