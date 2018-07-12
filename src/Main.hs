{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Nock
import System.Exit (exitSuccess)

main :: IO ()
main = do
  input <- T.getContents

  when (T.length input == 0) $ do
    T.putStrLn "USAGE: echo EXPR | ./hnock"
    exitSuccess

  let parsed = parse input

  case parsed of
    Left err   -> T.putStrLn (T.pack (show err))
    Right expr -> T.putStrLn (T.pack (show (nock expr)))

