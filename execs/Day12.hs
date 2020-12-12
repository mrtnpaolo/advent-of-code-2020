{-# OPTIONS_GHC -Wno-missing-signatures -Wno-type-defaults -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import Advent
import Advent.Coord
import Data.List (foldl')

main :: IO ()
main =
  do i <- getParsedLines parse 12
     --i <- getParsedTestLines parse 12 1
     --print `mapM_` [ (c,n) | (c,n) <- i, c `elem` "LR" ]
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

step (d,C y x) ('N',n) = (d,C (y-n) x)
step (d,C y x) ('S',n) = (d,C (y+n) x)
step (d,C y x) ('E',n) = (d,C y (x+n))
step (d,C y x) ('W',n) = (d,C y (x-n))
step (d,c) ('L',90)    = (l d,c)
step (d,c) ('L',180)   = (l (l d),c)
step (d,c) ('L',270)   = (l (l (l d)),c)
step (d,c) ('R',90)    = (r d,c)
step (d,c) ('R',180)   = (r (r d),c)
step (d,c) ('R',270)   = (r (r (r d)),c)
step (d,c) ('F',n)     = (d,forward d n c)

forward 'N' n (C y x) = C (y-n) x
forward 'E' n (C y x) = C y (x+n)
forward 'S' n (C y x) = C (y+n) x
forward 'W' n (C y x) = C y (x-n)

part2 ins = {- scanl step2 start ins -} manhattan (C 0 0) (fst end)
  where
    start = (C 0 0,C (-1) 10)
    end = foldl' step2 start ins

step2 (s,C wy wx) ('N',n) = (s,C (wy-n) wx)
step2 (s,C wy wx) ('S',n) = (s,C (wy+n) wx)
step2 (s,C wy wx) ('E',n) = (s,C wy (wx+n))
step2 (s,C wy wx) ('W',n) = (s,C wy (wx-n))
step2 (C sy sx,C wy wx) ('F',n) = (C sy' sx',C wy wx)
  where
    sy' = sy + n * wy
    sx' = sx + n * wx
step2 (s,w) ('L',(`div`90) ->n) = (s, rotate 'L' n w)
step2 (s,w) ('R',(`div`90) ->n) = (s, rotate 'R' n w)

--  y  x   y  x    y  x     y  x
-- -3  2   2  3    3 -2    -2 -3
--  x -y   x -y    x -y     x -y
rightangle (C y x) = C x (-y)

rotateright 1 c = rightangle c
rotateright 2 c = rightangle (rightangle c)
rotateright 3 c = rightangle (rightangle (rightangle c))
rotateleft 1 = rotateright 3
rotateleft 2 = rotateright 2
rotateleft 3 = rotateright 1

rotate 'R' n = rotateright n
rotate 'L' n = rotateleft n
