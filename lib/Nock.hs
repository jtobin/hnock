{-# OPTIONS_GHC -Wall #-}

module Nock (
    module L
  , module P

  , nock
  , hnock

  , E.eval
  ) where

import Data.Text as T
import qualified Nock.Eval as E
import Nock.Language as L
import Nock.Parse as P

hnock :: T.Text -> Noun
hnock input = case runParser expr [] "ghci" input of
  Left perr -> error (show perr)
  Right ex  -> case E.eval ex of
    Left err -> error (show err)
    Right e  -> e

nock :: Noun -> Noun
nock noun = case E.nock noun of
  Left err -> error (show err)
  Right e  -> e

