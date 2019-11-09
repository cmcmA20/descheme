module Scheme.Parse

import Data.List
import Data.Vect
import Data.String

import Scheme.DParser
import Scheme.Core

%hide Data.Vect.Here
%hide Data.Vect.There

%default total
%access public export

spaces : Parser ()
spaces = skipSome $ char ' '

special : Parser Char
special = oneOf $ unpack "!&|*+-/:<=?>@^_~"

escapedBackslashOrDoubleQuote : Parser (NonEmptyList Char)
escapedBackslashOrDoubleQuote = do
  char '\\'
  x <- oneOf $ unpack "\\\""
  pure $ MkNEList ([x] ** IsNonEmpty)

parseChar : Parser SExp
parseChar = do
  char '#'
  char '\\'
  name <- exactly (oneOf (unpack ".,;$%#\\\"'[]{}()") <|> special <|> digit) <|> some letter
  case name of
       (MkNEList (c :: [] ** _)) => pure $ SEChar c
       (MkNEList (c :: cs ** _)) => case toLower $ pack $ c :: cs of
                                         "space"   => pure $ SEChar ' '
                                         "tab"     => pure $ SEChar '\t'
                                         "newline" => pure $ SEChar '\n'
                                         _         => noParse "Can't parse character"

parseStr : Parser SExp
parseStr = do
  char '"'
  xs <- many $ escapedBackslashOrDoubleQuote <|> (some $ noneOf $ unpack "\\\"")
  char '"'
  pure $ SEStr $ pack $ concat $ map neWeaken xs

parseBool : Parser SExp
parseBool = do
  char '#'
  MkOO (_ ** prf) <- oneOf' $ unpack "tf"
  pure $ case prf of
              Here         => SEBool True
              There Here   => SEBool False
              There (There _) impossible

parseSymbol : Parser SExp
parseSymbol =
  [| (SESymbol . pack)
     [| (letter <|> special) :: (many (letter <|> digit <|> special)) |]
  |]

-- TODO exact/inexact, base modifier, numeric hierarchy
parseNum : Parser SExp
parseNum = do
  msign <- optional $ oneOf' $ unpack "+-"
  xs    <- parsePositive . pack . neWeaken <$> some digit
  case xs of
       Nothing => noParse "Can't parse number"
       Just n  => pure $ SENum $ SNExactInt $ case msign of
                                                   Nothing                => n
                                                   Just (MkOO (_ ** prf)) => case prf of
                                                                                  Here         =>  n
                                                                                  There Here   => -n
                                                                                  There (There _) impossible

mutual
  parseExpr : Parser SExp
  parseExpr =   assert_total
            $   parseNum
            <|> parseStr
            <|> parseSymbol
            <|> parseBool
            <|> parseChar
            <|> parseShortForm "'"  "quote"
            <|> parseShortForm "`"  "quasiquote"
            <|> parseShortForm ",@" "unquote-splicing"
            <|> parseShortForm ","  "unquote"
            <|> parseShortForm "λ"  "lambda"
            <|> parseListLike

  parseShortForm : String -> String -> Parser SExp
  parseShortForm short long = do
    string $ unpack short
    x <- parseExpr
    pure $ SECons (SESymbol long) x

  parseListLike : Parser SExp
  parseListLike = do
    mv <- optional $ char '#'
    char '('
    xs <- sepBy parseExpr spaces
    mx <- optional $ char '.' *> spaces *> parseExpr
    char ')'
    pure $ case mv of
         Nothing => unListify (xs, mx)
         Just _  => SEVect $ fromList $ xs ++ case mx of
                                                   Nothing => []
                                                   Just x  => [SESymbol ".", x]

runParseExpr : String -> Either String SExp
runParseExpr inp =
  case parse parseExpr inp of
       Left  err       => Left err
       Right (r, rest) => case unpack rest of
                               []        => Right r
                               (x :: xs) => Left $ "Trailing garbage: " ++ show rest

