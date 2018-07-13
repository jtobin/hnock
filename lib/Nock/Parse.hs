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
  op <- P.oneOf "?+=/*"
  case op of
    '?' -> fmap Wut noun
    '+' -> fmap Lus noun
    '=' -> fmap Tis noun
    '/' -> fmap Fas noun
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
    (h:t) -> case h of
      '0' -> case t of
        [] -> return (Atom 0)
        _  -> fail "atom: bad parse"

      _   ->
        let nat = read digits
        in  return (Atom nat)

    [] -> fail "atom: bad parse"

cell :: Monad m => P.ParsecT T.Text u m Noun
cell = do
  P.char '['
  P.skipMany P.space
  leader <- noun
  P.skipMany P.space
  rest <- P.sepBy noun (P.many1 P.space)
  P.skipMany P.space
  P.char ']'

  return (toCell (leader : rest))

toCell :: [Noun] -> Noun
toCell = loop where
  loop list = case list of
    []     -> error "cell: bad parse"
    [_]    -> error "cell: bad parse"
    [s, f] -> Cell s f
    (h:t)  -> Cell h (loop t)

