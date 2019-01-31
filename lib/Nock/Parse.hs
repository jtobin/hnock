{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Nock.Parse (
    P.runParser

  , parse
  , expr
  ) where

import Nock.Language
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Text.Parsec as P

parse :: T.Text -> Either P.ParseError Expr
parse = P.runParser expr [] "input"

expr :: Monad m => P.ParsecT T.Text u m Expr
expr =
      P.try operator
  <|> fmap Noun noun

operator :: Monad m => P.ParsecT T.Text u m Expr
operator = do
  op <- P.oneOf "?+=/#*"
  case op of
    '?' -> fmap Wut noun
    '+' -> fmap Lus noun
    '=' -> fmap Tis noun
    '/' -> fmap Net noun
    '#' -> fmap Hax noun
    '*' -> fmap Tar noun
    _   -> fail "op: bad token"

noun :: Monad m => P.ParsecT T.Text u m Noun
noun =
      P.try cell
  <|> atom

atom :: Monad m => P.ParsecT T.Text u m Noun
atom = do
  digits <- P.many P.digit
  case digits of
    ('0':t) -> case t of
      [] -> return (Atom 0)
      _  -> fail "atom: bad input"

    (_:_) ->
      let nat = read digits
      in  return (Atom nat)

    [] -> fail "atom: bad input"

cell :: Monad m => P.ParsecT T.Text u m Noun
cell = do
  P.char '['
  P.skipMany P.space
  h <- noun
  P.skipMany P.space
  t <- P.sepBy noun (P.many1 P.space)
  P.skipMany P.space
  P.char ']'

  return (toCell (h : t))

toCell :: [Noun] -> Noun
toCell = loop where
  loop list = case list of
    []     -> error "cell: bad input"
    [_]    -> error "cell: bad input"
    [s, f] -> Cell s f
    (h:t)  -> Cell h (loop t)

