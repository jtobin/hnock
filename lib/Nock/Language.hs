module Nock.Language (
    Noun(..)
  , Expr(..)
  ) where

data Noun =
    Atom Integer
  | Cell Noun Noun
  deriving Eq

instance Show Noun where
  show noun = case noun of
    Atom m   -> show m
    Cell m n -> mconcat ["[", show m, " ", show n, "]"]

data Expr =
    Noun !Noun
  | Wut !Noun
  | Lus !Noun
  | Tis !Noun
  | Net !Noun
  | Hax !Noun
  | Tar !Noun
  deriving Eq

instance Show Expr where
  show op = case op of
    Noun n -> show n
    Wut n  -> mconcat ["?", show n]
    Lus n  -> mconcat ["+", show n]
    Tis n  -> mconcat ["=", show n]
    Net n  -> mconcat ["/", show n]
    Hax n  -> mconcat ["#", show n]
    Tar n  -> mconcat ["*", show n]

