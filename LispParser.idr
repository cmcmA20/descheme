module LispParser

import Data.List
import Data.String
import Parser
import LispCore

%default total
%access public export

namespace LispParser

  escapedBackslashOrDoubleQuote : Parser (NonEmptyList Char)
  escapedBackslashOrDoubleQuote = do
    char '\\'
    x <- oneOf $ unpack "\\\""
    pure $ MkNEList ([x] ** IsNonEmpty)

  parseChar : Parser LispV
  parseChar = do
    char '#'
    char '\\'
    name <- exactly (char ')') <|> some (alphaNum <|> symbol <|> oneOf (unpack "#\\\"'[]{}("))
    case name of
         (MkNEList (c :: [] ** _)) => pure $ LVChar c
         (MkNEList (c :: cs ** _)) => case toLower (pack (c :: cs)) of
                                           "space"   => pure $ LVChar ' '
                                           "tab"     => pure $ LVChar '\t'
                                           "newline" => pure $ LVChar '\n'
                                           _         => noParse "Can't parse character"

  parseStr : Parser LispV
  parseStr = do
    char '"'
    xs <- many $ escapedBackslashOrDoubleQuote <|> (some $ noneOf $ unpack "\\\"")
    char '"'
    pure $ LVStr $ pack $ concat $ map neWeaken xs

  parseBool : Parser LispV
  parseBool = do
    char '#'
    MkOO (_ ** prf) <- oneOf' $ unpack "tf"
    pure $ case prf of
                Here         => LVBool True
                There Here   => LVBool False
                There (There _) impossible

  parseAtom : Parser LispV
  parseAtom =
    [| (LVAtom . pack)
       [| (letter <|> symbol) :: (many (letter <|> digit <|> symbol)) |]
    |]

  parseInt : Parser LispV
  parseInt = do
    msign <- optional $ oneOf' $ unpack "+-"
    xs    <- parsePositive . pack . neWeaken <$> some digit
    case xs of
         Nothing => noParse "Can't parse number"
         Just n  => pure $ LVInt $ case msign of
                                        Nothing                => n
                                        Just (MkOO (_ ** prf)) => case prf of
                                                                       Here         =>  n
                                                                       There Here   => -n
                                                                       There (There _) impossible

  mutual
    parseExpr : Parser LispV
    parseExpr =   assert_total
              $   parseInt
              <|> parseStr
              <|> parseAtom
              <|> parseBool
              <|> parseChar
              <|> parseQuoted
              <|> parseAnyList

    parseQuoted : Parser LispV
    parseQuoted = do
      char '\''
      x <- parseExpr
      pure $ LVList [LVAtom "quote", x]

    parseAnyList : Parser LispV
    parseAnyList = do
      char '('
      xs <- sepBy parseExpr spaces
      mx <- optional $ char '.' *> spaces *> parseExpr
      char ')'
      pure $ case mx of
                  Nothing => LVList xs
                  Just x  => LVDotList xs x

  runParseExpr : String -> Either String LispV
  runParseExpr inp =
    case parse parseExpr inp of
         Left  err       => Left err
         Right (r, rest) => case unpack rest of
                                 []        => Right r
                                 (x :: xs) => Left $ "Trailing garbage: " ++ show rest
