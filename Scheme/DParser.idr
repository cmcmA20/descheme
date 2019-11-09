module Scheme.DParser

import Data.List

%default total
%access public export

namespace Helpers

  data NonEmptyList : Type -> Type where
    MkNEList : (xs : List a ** NonEmpty xs) -> NonEmptyList a

  -- Functor NonEmptyList where
  --   map f (MkNEList (x :: xs ** IsNonEmpty)) = MkNEList (f x :: map f xs ** IsNonEmpty)

  neWeaken : NonEmptyList a -> List a
  neWeaken (MkNEList (xs ** _)) = xs

  neLeftApp : NonEmptyList a -> List a -> NonEmptyList a
  neLeftApp (MkNEList ((x :: xs) ** _)) ys = MkNEList (x :: (xs ++ ys) ** IsNonEmpty)

  neRightApp : List a -> NonEmptyList a -> NonEmptyList a
  neRightApp []        ys = ys
  neRightApp (x :: xs) ys = MkNEList (x :: (xs ++ neWeaken ys) ** IsNonEmpty)

  neBothApp : NonEmptyList a -> NonEmptyList a -> NonEmptyList a
  neBothApp xs ys = neRightApp (neWeaken xs) ys

  Semigroup (NonEmptyList a) where
    (<+>) = neBothApp

  data EqTo : {a : Type} -> (t : a) -> Type where
    MkET : (x : a ** x = t) -> EqTo t

  etWeaken : {t : a} -> EqTo t -> a
  etWeaken (MkET (x ** _)) = x

  data OneOf : {a : Type} -> List a -> Type where
    MkOO : (o : a ** Elem o os) -> OneOf os

  ooWeaken : {os : List a} -> OneOf os -> a
  ooWeaken (MkOO (o ** _)) = o

  eqPromoteToChoice : EqTo t -> OneOf [t]
  eqPromoteToChoice (MkET (x ** prf)) = MkOO (x ** rewrite sym prf in Here)

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
    (<+>) u v = [| u <+> v |]

  Monoid a => Monoid (Parser a) where
    neutral = MkParser $ \inp => Right (neutral, inp)

  noParse : String -> Parser a
  noParse err = MkParser $ \_ => Left err

  infixr 10 <?>
  (<?>) : Parser a -> String -> Parser a
  (<?>) u err = u <|> noParse err

  optional : Parser a -> Parser (Maybe a)
  optional u =
    MkParser $ \inp =>
    case parse u inp of
         Left  err       => Right (Nothing, inp )
         Right (r, inp') => Right (Just r , inp')

  many : Parser a -> Parser (List a)
  many u = do
    r <- optional u
    case r of
         Nothing => neutral
         Just x  => [| pure x :: assert_total (many u) |]

  some : Parser a -> Parser (NonEmptyList a)
  some u = do
    x  <- u
    xs <- many u
    pure $ MkNEList (x :: xs ** IsNonEmpty)

  exactly : Parser a -> Parser (NonEmptyList a)
  exactly u = do
    x <- u
    pure $ MkNEList ([x] ** IsNonEmpty)

  notFollowedBy : Parser a -> Parser ()
  notFollowedBy u =
    MkParser $ \inp =>
    case parse u inp of
         Left  _         => Right ((), inp)
         Right (_, inp') => Left $ "Shouldn't have parsed a token. Rest of the input: " ++ show inp'

  satisfy : (Char -> Bool) -> Parser Char
  satisfy p =
    MkParser $ \inp =>
    case unpack inp of
         []        => Left "Unexpected EOF"
         (x :: xs) => case p x of
                           False => Left $ "Can't match char '" ++ pack [x] ++ "'"
                           True  => Right (x, pack xs)

  anyChar : Parser Char
  anyChar = satisfy (const True)

  eof : Parser ()
  eof = notFollowedBy anyChar

  char : Char -> Parser Char
  char c = satisfy (c ==)

  notChar : Char -> Parser Char
  notChar c = satisfy (c /=)

  oneOf : List Char -> Parser Char
  oneOf cs = satisfy (\c => elem c cs)

  noneOf : List Char -> Parser Char
  noneOf cs = satisfy (\c => not $ elem c cs)

  string : List Char -> Parser (List Char)
  string = sequence . map char

  sepBy1 : Parser a -> Parser sep -> Parser (NonEmptyList a)
  sepBy1 u v = do
    x  <- u
    xs <- many (v *> u)
    pure $ MkNEList (x :: xs ** IsNonEmpty)

  sepBy : Parser a -> Parser sep -> Parser (List a)
  sepBy u v = map neWeaken (sepBy1 u v) <|> neutral

  endBy1 : Parser a -> Parser sep -> Parser (NonEmptyList a)
  endBy1 u v = sepBy1 u v <* v

  endBy : Parser a -> Parser sep -> Parser (List a)
  endBy u v = map neWeaken (endBy1 u v) <|> neutral

  skipMany : Parser a -> Parser ()
  skipMany u = many u *> pure ()

  skipSome : Parser a -> Parser ()
  skipSome u = some u *> pure ()

  -- Concrete useful parsers

  lower : Parser Char
  lower = satisfy isLower

  upper : Parser Char
  upper = satisfy isUpper

  letter : Parser Char
  letter = satisfy isAlpha

  digit : Parser Char
  digit = satisfy isDigit

  alphaNum : Parser Char
  alphaNum = satisfy isAlphaNum


namespace DependentParser

  char' : (c : Char) -> Parser (EqTo c)
  char' c =
    MkParser $ \inp =>
    case unpack inp of
         []        => Left "Unexpected EOF"
         (x :: xs) => case (decEq x c) of
                           Yes prf => Right (MkET (x ** prf), pack xs)
                           No  _   => Left $ "Can't match char '" ++ pack [x] ++ "'"

  string' : (s : List Char) -> Parser (EqTo s)
  string' []        = pure $ (MkET ([] ** Refl))
  string' (c :: cs) = do
    MkET (x  ** p) <- char' c
    MkET (xs ** q) <- string' cs
    pure $ MkET $
      rewrite sym p in
      rewrite sym q in
      (x :: xs ** Refl)

  oneOf' : (cs : List Char) -> Parser (OneOf cs)
  oneOf' cs =
    MkParser $ \inp =>
    case unpack inp of
         []        => Left "Unexpected EOF"
         (x :: xs) => case (isElem x cs) of
                           Yes prf => Right (MkOO (x ** prf), pack xs)
                           No  _   => Left $ "Can't match char '" ++ pack [x] ++ "'"

  infixr 10 <%>
  ||| like <|> but preserves restrictions
  (<%>): Parser (EqTo w) -> Parser (EqTo z) -> Parser (OneOf [w, z])
  (<%>) u v =
    MkParser $ \inp =>
    case parse u inp of
         Left  _ => case parse v inp of
                         Left  _ => Left "Can't match both chars" -- FIXME
                         Right (MkET (x ** prf), inp') => Right (rewrite sym prf in MkOO (x ** There Here), inp')
         Right (MkET (x ** prf), inp') => Right (rewrite sym prf in MkOO (x ** Here), inp')

  -- TODO rethink the restrictions in terms of boolean predicates so we can just compute some && and ||
  -- Too much structure, the order of elements is irrelevant here
  -- strangeConsOr : Parser (EqTo w) -> Parser (OneOf zs) -> Parser (OneOf (w :: zs))
  -- strangeConsApp : Parser (OneOf ws) -> Parser (OneOf zs) -> Parser (OneOf (ws ++ zs))
  -- seq' : {s1, s2 : List Char} -> Parser (EqTo s1) -> Parser (EqTo s2) -> 

