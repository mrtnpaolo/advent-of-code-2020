{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Advent
  ( module Advent
  ) where

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (foldl')

getRawInput :: Int {- ^ day number -} -> IO String
getRawInput n =
  do args <- getArgs
     case args of
       []    -> readFile (printf "inputs/input-%02d.txt" n)
       "-":_ -> getContents
       fn:_  -> readFile fn

getRawTest :: Int {- ^ day number -} -> Int {- ^ test number -} -> IO String
getRawTest n m =
  do args <- getArgs
     case args of
       []    -> readFile (printf "inputs/input-%02d-test-%02d.txt" n m)
       "-":_ -> getContents

count :: Foldable f => (a -> Bool) -> f a -> Int
count p = foldl' (\n x -> if p x then n+1 else n) 0
