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

getParsed :: (String -> a) -> Int -> IO a
getParsed f n = f <$> getRawInput n

getParsedLines :: (String -> a) -> Int -> IO [a]
getParsedLines f n = map f . lines <$> getRawInput n

getRawTest :: Int {- ^ day number -} -> Int {- ^ test number -} -> IO String
getRawTest n m =
  do args <- getArgs
     case args of
       []    -> readFile (printf "inputs/input-%02d-test-%02d.txt" n m)
       "-":_ -> getContents

getParsedTest :: (String -> a) -> Int -> Int -> IO a
getParsedTest f n i = f <$> getRawTest n i

getParsedTestLines :: (String -> a) -> Int -> Int -> IO [a]
getParsedTestLines f n i = map f . lines <$> getRawTest n i

count :: Foldable f => (a -> Bool) -> f a -> Int
count p = foldl' (\n x -> if p x then n+1 else n) 0
