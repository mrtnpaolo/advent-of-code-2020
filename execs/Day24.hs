module Main
  ( main
  ) where

import Advent    (getInput,count)

import Data.List (foldl')

import Data.Set           qualified as  S
import Data.Map.Strict    qualified as  M

main :: IO ()
main =
  do i <- getInput parse 24
     let odds = M.keysSet
              . M.filter odd
              . M.fromListWith (+)
              $ [ (c,1) | c <- map walk i ]
     print (part1 odds)
     print (part2 odds)

data HDIR = E | SE | SW | W | NW | NE deriving (Show,Eq,Ord)

parse = map pos . lines
  where
    pos [] = []
    pos (    'e':r) =  E : pos r
    pos ('s':'e':r) = SE : pos r
    pos ('s':'w':r) = SW : pos r
    pos (    'w':r) =  W : pos r
    pos ('n':'w':r) = NW : pos r
    pos ('n':'e':r) = NE : pos r
    pos _ = undefined

data HC = HC !Int !Int !Int deriving (Show,Eq,Ord)

step (HC x y z)  E = HC (x+1) (y-1) z
step (HC x y z) SE = HC x (y-1) (z+1)
step (HC x y z) SW = HC (x-1) y (z+1)
step (HC x y z)  W = HC (x-1) (y+1) z
step (HC x y z) NW = HC x (y+1) (z-1)
step (HC x y z) NE = HC (x+1) y (z-1)

part1 = S.size

walk = foldl' step (HC 0 0 0)

nbs c = zipWith step (repeat c) [E,SE,SW,W,NW,NE]

part2 = S.size . (!! 100) . iterate tick

tick odds = S.filter dead odds `S.union` new
  where
    dead c = ncount == 1 || ncount == 2
      where
        ncount = count (`S.member` odds) (nbs c)
    new = S.fromList $
      [ c | c <- concatMap nbs (S.toList odds)
          , c `S.notMember` odds
          , 2 == length [ c' | c' <- nbs c, c' `S.member` odds ] ]
