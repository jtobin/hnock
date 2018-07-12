
module Nock.Language (
    Noun(..)
  , Expr(..)
  ) where

data Noun =
    Atom !Int
  | Cell !Noun !Noun
  deriving Eq

instance Show Noun where
  show noun = case noun of
    Atom m   -> show m
    Cell m n -> mconcat ["[", show m, " ", show n, "]"]

data Expr =
    Noun !Noun
  | Wut !Expr
  | Lus !Expr
  | Tis !Expr
  | Fas !Expr
  | Tar !Expr
  deriving Eq

instance Show Expr where
  show op = case op of
    Noun n -> show n
    Wut n  -> mconcat ["?", show n]
    Lus n  -> mconcat ["+", show n]
    Tis n  -> mconcat ["=", show n]
    Fas n  -> mconcat ["/", show n]
    Tar n  -> mconcat ["*", show n]

