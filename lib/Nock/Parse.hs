{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Nock.Parse (
    parse
  ) where

import Nock.Language
import qualified Text.Parsec as P
import qualified Data.Text as T
import Control.Applicative ((<|>))

parse :: T.Text -> Either P.ParseError Expr
parse = P.runParser expr [] "input"

expr :: Monad m => P.ParsecT T.Text u m Expr
expr =
      P.try operator
  <|> P.try cell
  <|> atom

atom :: Monad m => P.ParsecT T.Text u m Expr
atom = do
  digits <- P.many P.digit
  case digits of
    (h:t) -> case h of
      '0' -> case t of
        [] -> return (Noun (Atom 0))
        _  -> fail "atom: bad parse"

      _   ->
        let nat = read digits
        in  return (Noun (Atom nat))

    [] -> fail "atom: bad parse"

operator :: Monad m => P.ParsecT T.Text u m Expr
operator = do
  op <- P.oneOf "?+=/*"
  case op of
    '?' -> fmap Wut expr
    '+' -> fmap Lus expr
    '=' -> fmap Tis expr
    '/' -> fmap Fas expr
    '*' -> fmap Tar expr
    _   -> fail "op: bad token"

cell :: Monad m => P.ParsecT T.Text u m Expr
cell = do
  P.char '['
  P.skipMany P.space
  leader <- expr
  P.skipMany P.space
  rest <- P.sepBy expr (P.many1 P.space)
  P.skipMany P.space
  P.char ']'

  return (toPair (leader : rest))

toPair :: [Expr] -> Expr
toPair = loop where
  loop list = case list of
    []     -> error "cell: bad parse"
    [_]    -> error "cell: bad parse"
    [s, f] -> Pair s f
    (h:t)  -> Pair h (loop t)

