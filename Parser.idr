module Parser

%default total
%access public export

namespace NonEmptyList

  data NonEmptyList : Type -> Type where
    MkNEList : (xs : List a ** NonEmpty xs) -> NonEmptyList a

  neWeaken : NonEmptyList a -> List a
  neWeaken (MkNEList (xs ** _)) = xs

  neApp : NonEmptyList a -> List a -> NonEmptyList a
  neApp (MkNEList ((x :: xs) ** _)) ys = MkNEList (x :: (xs ++ ys) ** IsNonEmpty)

namespace Parser

  ||| Left values are for parse errors
  ||| Right values are parsed symbols and the rest of the string
  record Parser a where
    constructor MkParser
    parse : String -> Either String (a, String)

  Functor Parser where
    map f (MkParser u) =
      MkParser $ \inp =>
      case u inp of
           Left  err       => Left err
           Right (r, inp') => Right (f r, inp')

  Applicative Parser where
    pure x =
      MkParser $ \inp =>
      Right (x, inp)
    (<*>) (MkParser f) u =
      MkParser $ \inp =>
      case f inp of
           Left  err        => Left err
           Right (f', inp') => parse (map f' u) inp'

  ||| Left-biased "or"
  Alternative Parser where
    empty = MkParser $ const $ Left "No parse"
    (<|>) (MkParser u) (MkParser v) =
      MkParser $ \inp =>
      case u inp of
           Left  _   => v inp
           Right res => Right res

  Monad Parser where
    (>>=) u f =
      MkParser $ \inp =>
      case parse u inp of
           Left  err       => Left err
           Right (r, inp') => parse (f r) inp'

  Semigroup a => Semigroup (Parser a) where
    (<+>) u v = (<+>) <$> u <*> v

  Monoid a => Monoid (Parser a) where
    neutral = MkParser $ \inp => Right (neutral, inp)

  never : String -> Parser a
  never err = MkParser $ \_ => Left err

  eof : Parser ()
  eof =
    MkParser $ \inp =>
    case unpack inp of
         []       => Right ((), inp)
         (x :: _) => Left $ "Expected EOF, got char '" ++ pack [x] ++ "'"

  anyChar : Parser Char
  anyChar =
    MkParser $ \inp =>
    case unpack inp of
         []        => Left "Unexpected EOF"
         (x :: xs) => Right (x, pack xs)

  char : Char -> Parser Char
  char c =
    MkParser $ \inp =>
    case unpack inp of
         []        => Left "Unexpected EOF"
         (x :: xs) => case decEq c x of
                           No  _ => Left $ "Can't match char '" ++ pack [x] ++ "' with '" ++ pack [c] ++ "'"
                           Yes _ => Right (x, pack xs)

  notChar : Char -> Parser Char
  notChar c =
    MkParser $ \inp =>
    case unpack inp of
         []        => Left "Unexpected EOF"
         (x :: xs) => case parse (char c) inp of
                           Left  _ => Right (x, pack xs)
                           Right _ => Left $ "Can't match char '" ++ pack [x] ++ "' with '" ++ pack [c] ++ "'"

  oneOf : List Char -> Parser Char
  oneOf = choice . map char

  noneOf : List Char -> Parser Char
  noneOf cs =
    MkParser $ \inp =>
    case unpack inp of
         []        => Left "Unexpected EOF"
         (x :: xs) => case parse (oneOf cs) inp of
                           Left  _ => Right (x, pack xs)
                           Right _ => Left $ "Can't match char '" ++ pack [x] ++ "' with one of \"" ++ pack cs ++ "\""

  many : Parser a -> Parser (List a)
  many u =
    MkParser $ \inp =>
    case parse u inp of
         Left  _         => Right ([], inp)
         Right (r, inp') => map (\(l, s) => (r :: l, s)) $ assert_total $ parse (many u) inp'

  many1 : Parser a -> Parser (NonEmptyList a)
  many1 u = do
    x <- u
    xs <- many u
    pure $ MkNEList (x :: xs ** IsNonEmpty)

  sepBy1 : Parser a -> Parser sep -> Parser (NonEmptyList a)
  sepBy1 u v = do
    x <- u
    xs <- many (v *> u)
    pure $ MkNEList (x :: xs ** IsNonEmpty)

  sepBy : Parser a -> Parser sep -> Parser (List a)
  sepBy u v = map neWeaken (sepBy1 u v) <|> neutral

  endBy1 : Parser a -> Parser sep -> Parser (NonEmptyList a)
  endBy1 u v = sepBy1 u v <* v

  endBy : Parser a -> Parser sep -> Parser (List a)
  endBy u v = map neWeaken (endBy1 u v) <|> neutral

  skipMany : Parser a -> Parser ()
  skipMany u = do
    xs <- many u
    pure ()

  skipMany1 : Parser a -> Parser ()
  skipMany1 u = do
    x <- u
    skipMany u

  -- Concrete useful parsers

  space : Parser Char
  space = char ' '

  symbol : Parser Char
  symbol = oneOf $ unpack "!$%&|*+-/:<=?>@^_~"

  parens : Parser Char
  parens = oneOf $ unpack "()[]{}"

  lower : Parser Char
  lower = oneOf $ unpack "qwertyuiopasdfghjklzxcvbnm"

  upper : Parser Char
  upper = oneOf $ unpack "QWERTYUIOPASDFGHJKLZXCVBNM"

  letter : Parser Char
  letter = lower <|> upper

  digit : Parser Char
  digit = oneOf $ unpack "0123456789"

  spaces : Parser ()
  spaces = skipMany1 space

  alphaNum : Parser Char
  alphaNum = letter <|> digit
