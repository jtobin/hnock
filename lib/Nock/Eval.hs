
module Nock.Eval (
    nock
  ) where

import Nock.Language

nock :: Expr -> Noun
nock expr = case expr of
  Noun noun -> noun
  Wut  e -> wut (nock e)
  Lus  e -> lus (nock e)
  Tis  e -> tis (nock e)
  Fas  e -> fas (nock e)
  Tar  e -> tar (nock e)

wut :: Noun -> Noun
wut noun = case noun of
  Cell {} -> Atom 0
  Atom {} -> Atom 1

lus :: Noun -> Noun
lus noun = case noun of
  Cell {} -> error "lus: bad noun"
  Atom m  -> Atom (1 + m)

tis :: Noun -> Noun
tis noun = case noun of
  Atom {}  -> error "tis: bad noun"
  Cell m n ->
    if   m == n
    then Atom 0
    else Atom 1

fas :: Noun -> Noun
fas noun = case noun of
  Atom {}  -> error "fas: bad noun"
  Cell m n -> case m of
    Cell {} -> error "fas: bad noun"
    Atom a  -> case a of

      1 -> n

      2 -> case n of
        Atom {}  -> error "fas: bad noun"
        Cell o _ -> o

      3 -> case n of
        Atom {}  -> error "fas: bad noun"
        Cell _ o -> o

      _ ->
        if    even a
        then
          let inner = Cell (Atom (a `div` 2)) n
          in  fas (Cell (Atom 2) (fas inner))
        else
          let inner = Cell (Atom ((a - 1) `div` 2)) n
          in  fas (Cell (Atom 3) (fas inner))

tar :: Noun -> Noun
tar noun = case noun of
  Atom {}  -> error "tar: bad noun"
  Cell a m -> case m of
    Atom {} -> error "tar: bad noun"
    Cell n d -> case n of
      Cell b c -> Cell (tar (Cell a (Cell b c))) (tar (Cell a d))
      Atom z   -> case z of

        0 -> fas (Cell d a)

        1 -> d

        2 -> case d of
          Atom {}  -> error "tar: bad noun"
          Cell e f ->
            tar (Cell (tar (Cell a e)) (tar (Cell a f)))

        3 -> wut (tar (Cell a d))

        4 -> lus (tar (Cell a d))

        5 -> tis (tar (Cell a d))

        6 -> case d of
          Atom {}  -> error "tar: bad noun"
          Cell g h -> case h of
            Atom {}  -> error "tar: bad noun"
            Cell i j -> tar (tar6 a g i j)

        7 -> case d of
          Atom {}  -> error "tar: bad noun"
          Cell g h ->
            tar (Cell a (Cell (Atom 2) (Cell g (Cell (Atom 1) h))))

        8 -> case d of
          Atom {}  -> error "tar: bad noun"
          Cell g h -> tar (tar8 a g h)

        9 -> case d of
          Atom {}  -> error "tar: bad noun"
          Cell g h -> tar (tar9 a g h)

        10 -> case d of
          Atom {} -> error "tar: bad noun"
          Cell g h -> case g of
            Cell i j -> tar (tar10 a i j h)
            _        -> tar (Cell a h)

        _ -> error "tar: bad noun"

tar6 :: Noun -> Noun -> Noun -> Noun -> Noun
tar6 a b c d =
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

tar8 :: Noun -> Noun -> Noun -> Noun
tar8 a b c =
  Cell a
    (Cell (Atom 7)
      (Cell
        (Cell (Atom 7)
            (Cell (Cell (Atom 0) (Atom 1)) b))
        (Cell (Cell (Atom 0) (Atom 1)) c)))

tar9 :: Noun -> Noun -> Noun -> Noun
tar9 a b c =
  Cell a
    (Cell (Atom 7)
      (Cell c
        (Cell (Atom 2)
          (Cell (Cell (Atom 0) (Atom 1))
            (Cell (Atom 0) b)))))

tar10 :: Noun -> Noun -> Noun -> Noun -> Noun
tar10 a _ c d =
  Cell a
    (Cell (Atom 8)
      (Cell c
        (Cell (Atom 7)
          (Cell (Cell (Atom 0) (Atom 3))
            d))))
