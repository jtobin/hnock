module Nock.Eval (
    nock
  , eval
  ) where

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
  Net  e    -> net e
  Hax  e    -> hax e
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

net :: Noun -> Possibly Noun
net noun = case noun of
  Cell (Atom 1) a          -> return a
  Cell (Atom 2) (Cell a _) -> return a
  Cell (Atom 3) (Cell _ b) -> return b
  Cell (Atom a) b          ->
    if   even a
    then do
      inner <- net (Cell (Atom (a `div` 2)) b)
      net (Cell (Atom 2) inner)
    else do
      inner <- net (Cell (Atom ((a - 1) `div` 2)) b)
      net (Cell (Atom 3) inner)

  _ -> Left (Error noun)

hax :: Noun -> Possibly Noun
hax noun = case noun of
  Cell (Atom 1) (Cell a _) -> return a
  Cell (Atom a) (Cell b c) ->
    if   even a
    then do
      let e = a `div` 2
      inner <- net (Cell (Atom (e + e + 1)) c)
      hax (Cell (Atom e) (Cell (Cell b inner) c))
    else do
      let o = (a - 1) `div` 2
      inner <- net (Cell (Atom (o + o)) c)
      hax (Cell (Atom o) (Cell (Cell inner b) c))
  _ -> Left (Error noun)

tar :: Noun -> Possibly Noun
tar noun = case noun of
  Cell a (Cell (Cell b c) d) -> do
    inner0 <- tar (Cell a (Cell b c))
    inner1 <- tar (Cell a d)
    return (Cell inner0 inner1)

  Cell a (Cell (Atom 0) b) ->
    net (Cell b a)

  Cell _ (Cell (Atom 1) b) ->
    return b

  Cell a (Cell (Atom 2) (Cell b c)) -> do
    inner0 <- tar (Cell a b)
    inner1 <- tar (Cell a c)
    tar (Cell inner0 inner1)

  Cell a (Cell (Atom 3) b) -> do
    tard <- tar (Cell a b)
    wut tard

  Cell a (Cell (Atom 4) b) -> do
    tard <- tar (Cell a b)
    lus tard

  Cell a (Cell (Atom 5) (Cell b c)) -> do
    tard0 <- tar (Cell a b)
    tard1 <- tar (Cell a c)
    tis (Cell tard0 tard1)

  Cell a (Cell (Atom 6) (Cell b (Cell c d))) -> do
    tard0 <- tar (Cell a (Cell (Atom 4) (Cell (Atom 4) b)))
    tard1 <- tar (Cell (Cell (Atom 2) (Atom 3)) (Cell (Atom 0) tard0))
    tard2 <- tar (Cell (Cell c d) (Cell (Atom 0) tard1))
    tar (Cell a tard2)

  Cell a (Cell (Atom 7) (Cell b c)) -> do
    tard <- tar (Cell a b)
    tar (Cell tard c)

  Cell a (Cell (Atom 8) (Cell b c)) -> do
    tard <- tar (Cell a b)
    tar (Cell (Cell tard a) c)

  Cell a (Cell (Atom 9) (Cell b c)) -> do
    tard <- tar (Cell a c)
    tar (Cell tard
      (Cell (Atom 2) (Cell (Cell (Atom 0) (Atom 1)) (Cell (Atom 0) b))))

  Cell a (Cell (Atom 10) (Cell (Cell b c) d)) -> do
    tard0 <- tar (Cell a c)
    tard1 <- tar (Cell a d)
    hax (Cell b (Cell tard0 tard1))

  Cell a (Cell (Atom 11) (Cell (Cell _ c) d)) -> do
    tard0 <- tar (Cell a c)
    tard1 <- tar (Cell a d)
    tar (Cell (Cell tard0 tard1) (Cell (Atom 0) (Atom 3)))

  Cell a (Cell (Atom 11) (Cell _ c)) ->
    tar (Cell a c)

  _ -> Left (Error noun)

