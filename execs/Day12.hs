{-# OPTIONS_GHC -Wno-missing-signatures -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import Advent
import Advent.Coord
import Data.List (foldl')

main :: IO ()
main =
  do i <- getParsedLines parse 12
     print (part1 i)
     print (part2 i)

parse :: String -> (Char,Int)
parse (x:(read -> n)) = (x,n)
parse xs = error (show xs)

part1 ins = manhattan (snd start) (snd end)
  where
    start = ('E',C 0 0)
    end = foldl' step start ins

l 'N' = 'W'
l 'W' = 'S'
l 'S' = 'E'
l 'E' = 'N'
r 'N' = 'E'
r 'E' = 'S'
r 'S' = 'W'
r 'W' = 'N'

-- apply a function n>0 times
app 1 f x = f x
app n f x = let y = f x in y `seq` app (n-1) f y

step (d,C y x) ('N',n)        = (d,C (y-n) x)
step (d,C y x) ('S',n)        = (d,C (y+n) x)
step (d,C y x) ('E',n)        = (d,C y (x+n))
step (d,C y x) ('W',n)        = (d,C y (x-n))
step (d,c) ('L',(`div`90)->n) = (app n l d,c)
step (d,c) ('R',(`div`90)->n) = (app n r d,c)
step (d,c) ('F',n)            = (d,forward d n c)

forward 'N' n (C y x) = C (y-n) x
forward 'E' n (C y x) = C y (x+n)
forward 'S' n (C y x) = C (y+n) x
forward 'W' n (C y x) = C y (x-n)

part2 ins = manhattan (fst start) (fst end)
  where
    start = (C 0 0,C (-1) 10)
    end = foldl' step2 start ins

step2 (s,C wy wx) ('N',n)       = (s,C (wy-n) wx)
step2 (s,C wy wx) ('S',n)       = (s,C (wy+n) wx)
step2 (s,C wy wx) ('E',n)       = (s,C wy (wx+n))
step2 (s,C wy wx) ('W',n)       = (s,C wy (wx-n))
step2 (s,w) ('L',(`div`90) ->n) = (s, rotate 'L' n w)
step2 (s,w) ('R',(`div`90) ->n) = (s, rotate 'R' n w)
step2 (C sy sx,C wy wx) ('F',n) = (C sy' sx',C wy wx)
  where
    sy' = sy + n * wy
    sx' = sx + n * wx

clockwise (C y x) = C x (-y)

rotate 'L' n = app (4-n) clockwise
rotate 'R' n = app n     clockwise
