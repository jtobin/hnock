{-# OPTIONS_GHC -Wall #-}

module Nock (
    module X
  ) where

import Nock.Language as X
import Nock.Eval as X

-- test expressions -----------------------------------------------------------

-- /[3 [[4 5] [6 14 15]]]
-- should be [6 [14 15]]
test0 :: Expr
test0 =
  Fas
    (Noun (Cell (Atom 3)
      (Cell (Cell (Atom 4) (Atom 5))
        (Cell (Atom 6) (Cell (Atom 14) (Atom 15))))))

-- *[[[4 5] [6 14 15]] [0 7]]
-- should be [14 15]
test1 :: Expr
test1 =
  Tar
    (Noun (Cell
      (Cell (Cell (Atom 4) (Atom 5))
        (Cell (Atom 6) (Cell (Atom 14) (Atom 15))))
      (Cell (Atom 0) (Atom 7))))

-- *[42 [1 153 218]]
-- should be [153 218]
test2 :: Expr
test2 =
  Tar
    (Noun (Cell (Atom 42)
      (Cell (Atom 1) (Cell (Atom 153) (Atom 218)))))

-- *[57 [4 0 1]]
-- should be 58
test3 :: Expr
test3 =
  Tar
    (Noun (Cell (Atom 57)
      (Cell (Atom 4)
        (Cell (Atom 0) (Atom 1)))))

-- *[[132 19] [4 0 3]]
-- should be 20
test4 :: Expr
test4 =
  Tar
    (Noun (Cell (Cell (Atom 132) (Atom 19))
      (Cell (Atom 4)
        (Cell (Atom 0) (Atom 3)))))

-- /[7 [[4 5] [6 14 15]]]
-- should be [14 15]
test5 :: Expr
test5 =
  Fas
    (Noun (Cell (Atom 7)
      (Cell (Cell (Atom 4) (Atom 5))
        (Cell (Atom 6) (Cell (Atom 14) (Atom 15))))))

-- *[77 [2 [1 42] [1 1 153 218]]]
-- should be [153 218]
test6 :: Expr
test6 =
  Tar
    (Noun
    (Cell (Atom 77)
      (Cell (Atom 2)
        (Cell (Cell (Atom 1) (Atom 42))
          (Cell (Atom 1)
            (Cell (Atom 1)
              (Cell (Atom 153) (Atom 218))))))))

main :: IO ()
main = do
  print (nock test0)
  print (nock test1)
  print (nock test2)
  print (nock test3)
  print (nock test4)
  print (nock test5)
  print (nock test6)
