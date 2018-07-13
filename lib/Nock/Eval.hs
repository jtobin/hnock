module Nock.Eval (
    nock
  , eval
  ) where

import Control.Monad ((<=<))
import Nock.Language

data Error = Error Noun
  deriving Show

type Possibly = Either Error

nock :: Noun -> Possibly Noun
nock = tar

eval :: Expr -> Possibly Noun
eval expr = case expr of
  Noun noun -> return noun
  Wut  e    -> wut e
  Lus  e    -> lus e
  Tis  e    -> tis e
  Fas  e    -> fas e
  Tar  e    -> tar e

wut :: Noun -> Possibly Noun
wut noun = return $ case noun of
  Cell {} -> Atom 0
  Atom {} -> Atom 1

lus :: Noun -> Possibly Noun
lus noun = case noun of
  Cell {} -> Left (Error noun)
  Atom m  -> return (Atom (1 + m))

tis :: Noun -> Possibly Noun
tis noun = case noun of
  Atom {}  -> Left (Error noun)
  Cell m n -> return $
    if   m == n
    then Atom 0
    else Atom 1

fas :: Noun -> Possibly Noun
fas noun = case noun of
  Cell (Atom 1) a          -> return a
  Cell (Atom 2) (Cell a _) -> return a
  Cell (Atom 3) (Cell _ b) -> return b
  Cell (Atom a) b          ->
    if   even a
    then do
      inner <- fas (Cell (Atom (a `div` 2)) b)
      fas (Cell (Atom 2) inner)
    else do
      inner <- fas (Cell (Atom ((a - 1) `div` 2)) b)
      fas (Cell (Atom 3) inner)

  _ -> Left (Error noun)

tar :: Noun -> Possibly Noun
tar noun = case noun of
  Cell a (Cell (Cell b c) d) -> do
    inner0 <- tar (Cell a (Cell b c))
    inner1 <- tar (Cell a d)
    return (Cell inner0 inner1)

  Cell a (Cell (Atom 0) b) ->
    fas (Cell b a)

  Cell _ (Cell (Atom 1) b) ->
    return b

  Cell a (Cell (Atom 2) (Cell b c)) -> do
    inner0 <- tar (Cell a b)
    inner1 <- tar (Cell a c)
    tar (Cell inner0 inner1)

  Cell a (Cell (Atom 3) b) ->
    let wuttar = wut <=< tar
    in  wuttar (Cell a b)

  Cell a (Cell (Atom 4) b) ->
    let lustar = lus <=< tar
    in  lustar (Cell a b)

  Cell a (Cell (Atom 5) b) ->
    let tistar = tis <=< tar
    in  tistar (Cell a b)

  Cell a (Cell (Atom 6) (Cell b (Cell c d))) ->
    tar6 a b c d

  Cell a (Cell (Atom 7) (Cell b c)) ->
    tar (Cell a (Cell (Atom 2) (Cell b (Cell (Atom 1) c))))

  Cell a (Cell (Atom 8) (Cell b c)) ->
    tar8 a b c

  Cell a (Cell (Atom 9) (Cell b c)) ->
    tar9 a b c

  Cell a (Cell (Atom 10) (Cell (Cell b c) d)) ->
    tar10 a b c d

  Cell a (Cell (Atom 10) (Cell _ c)) ->
    tar (Cell a c)

  _ -> Left (Error noun)

tar6 :: Noun -> Noun -> Noun -> Noun -> Possibly Noun
tar6 a b c d = tar $
  Cell a
    (Cell (Atom 2)
    (Cell (Cell (Atom 0) (Atom 1))
    (Cell (Atom 2)
    (Cell (Cell (Atom 1) (Cell c d))
    (Cell (Cell (Atom 1) (Atom 0))
    (Cell (Atom 2)
    (Cell (Cell (Atom 1) (Cell (Atom 2) (Atom 3)))
    (Cell (Cell (Atom 1) (Atom 0))
    (Cell (Atom 4)
    (Cell (Atom 4) b))))))))))

tar8 :: Noun -> Noun -> Noun -> Possibly Noun
tar8 a b c = tar $
  Cell a
    (Cell (Atom 7)
    (Cell
    (Cell (Atom 7)
    (Cell (Cell (Atom 0) (Atom 1)) b))
    (Cell (Cell (Atom 0) (Atom 1)) c)))

tar9 :: Noun -> Noun -> Noun -> Possibly Noun
tar9 a b c = tar $
  Cell a
    (Cell (Atom 7)
    (Cell c
    (Cell (Atom 2)
    (Cell (Cell (Atom 0) (Atom 1))
    (Cell (Atom 0) b)))))

tar10 :: Noun -> Noun -> Noun -> Noun -> Possibly Noun
tar10 a _ c d = tar $
  Cell a
    (Cell (Atom 8)
    (Cell c
    (Cell (Atom 7)
    (Cell (Cell (Atom 0) (Atom 3))
    d))))

