{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Advent
  ( module Advent
  ) where

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (foldl')

getInput :: (String -> a) -> Int {- ^ day number -} -> IO a
getInput parse n =
  do args <- getArgs
     parse <$> case args of
       []    -> readFile (printf "inputs/input-%02d.txt" n)
       "-":_ -> getContents
       fn:_  -> readFile fn

getTest :: (String -> a)
        -> Int {- ^ day number -}
        -> Int {- ^ test number -}
        -> IO a
getTest parse n m =
  do args <- getArgs
     parse <$> case args of
       []    -> readFile (printf "inputs/input-%02d-test-%02d.txt" n m)
       "-":_ -> getContents

getInputLines :: (String -> a) -> Int -> IO [a]
getInputLines parse = getInput (map parse . lines)

getTestLines :: (String -> a) -> Int -> Int -> IO [a]
getTestLines parse = getTest (map parse . lines)

ri :: String -> Int
ri = read

count :: Foldable f => (a -> Bool) -> f a -> Int
count p = foldl' (\n x -> if p x then n+1 else n) 0

dup :: Eq a => [a] -> a
dup (x:y:_) | x == y = x
dup (_:xs)           = dup xs

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]
