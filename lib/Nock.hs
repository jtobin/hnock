{-# OPTIONS_GHC -Wall #-}

module Nock (
    module L
  , module P

  , enock
  , hnock

  , eval
  , nock
  ) where

import Data.Text as T
import Nock.Eval as E
import Nock.Language as L
import Nock.Parse as P

hnock :: T.Text -> Noun
hnock input = case runParser expr [] "ghci" input of
  Left perr -> error (show perr)
  Right ex  -> case eval ex of
    Left err -> error (show err)
    Right e  -> e

enock :: Noun -> Noun
enock noun = case nock noun of
  Left err -> error (show err)
  Right e  -> e

