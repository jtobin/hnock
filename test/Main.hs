module Main where

import Nock

-- /[3 [[4 5] [6 14 15]]]
-- should be [6 [14 15]]
expr0 :: Expr
expr0 =
  Fas
    (Cell (Atom 3)
      (Cell (Cell (Atom 4) (Atom 5))
        (Cell (Atom 6) (Cell (Atom 14) (Atom 15)))))

noun0 :: Noun
noun0 = Cell (Atom 6) (Cell (Atom 14) (Atom 15))

-- *[[[4 5] [6 14 15]] [0 7]]
-- should be [14 15]
expr1 :: Expr
expr1 =
  Tar
    (Cell
      (Cell (Cell (Atom 4) (Atom 5))
        (Cell (Atom 6) (Cell (Atom 14) (Atom 15))))
      (Cell (Atom 0) (Atom 7)))

noun1 :: Noun
noun1 = Cell (Atom 14) (Atom 15)

-- *[42 [1 153 218]]
-- should be [153 218]
expr2 :: Expr
expr2 =
  Tar
    (Cell (Atom 42)
      (Cell (Atom 1) (Cell (Atom 153) (Atom 218))))

noun2 :: Noun
noun2 = Cell (Atom 153) (Atom 218)

-- *[57 [4 0 1]]
-- should be 58
expr3 :: Expr
expr3 =
  Tar
    (Cell (Atom 57)
      (Cell (Atom 4)
        (Cell (Atom 0) (Atom 1))))

noun3 :: Noun
noun3 = Atom 58

-- *[[132 19] [4 0 3]]
-- should be 20
expr4 :: Expr
expr4 =
  Tar
    (Cell (Cell (Atom 132) (Atom 19))
      (Cell (Atom 4)
        (Cell (Atom 0) (Atom 3))))

noun4 :: Noun
noun4 = Atom 20

-- /[7 [[4 5] [6 14 15]]]
-- should be [14 15]
expr5 :: Expr
expr5 =
  Fas
    (Cell (Atom 7)
      (Cell (Cell (Atom 4) (Atom 5))
        (Cell (Atom 6) (Cell (Atom 14) (Atom 15)))))

noun5 :: Noun
noun5 = Cell (Atom 14) (Atom 15)

-- *[77 [2 [1 42] [1 1 153 218]]]
-- should be [153 218]
expr6 :: Expr
expr6 =
  Tar
    (Cell (Atom 77)
      (Cell (Atom 2)
        (Cell (Cell (Atom 1) (Atom 42))
          (Cell (Atom 1)
            (Cell (Atom 1)
              (Cell (Atom 153) (Atom 218)))))))

noun6 :: Noun
noun6 = Cell (Atom 153) (Atom 218)

main :: IO ()
main = do
  print (fmap (== noun0) (eval expr0))
  print (fmap (== noun1) (eval expr1))
  print (fmap (== noun2) (eval expr2))
  print (fmap (== noun3) (eval expr3))
  print (fmap (== noun4) (eval expr4))
  print (fmap (== noun5) (eval expr5))
  print (fmap (== noun6) (eval expr6))

