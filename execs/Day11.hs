module Main
  ( main
  ) where

import Advent (getInput,count,dup)
import Advent.Coord (Coord(..),neighbors)

import Data.Maybe (maybeToList,catMaybes,isJust)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main :: IO ()
main =
  do grid <- getInput parse 11
     print (part1 grid)
     print (part2 grid)

data Seat = Floor | Empty | Occupied
  deriving (Show,Eq)

type Grid = Map Coord Seat

parse :: String -> Grid
parse (lines -> rows) =
  M.fromList [ (C y x,the thing)
             | (y,cols)  <- zip [0..] rows
             , (x,thing) <- zip [0..] cols ]
  where
    the '.' = Floor
    the 'L' = Empty
    the '#' = Occupied
    the  _  = undefined

part1 :: Grid -> Int
part1 = M.size . M.filter (== Occupied) . dup . iterate tick

adj :: Grid -> Coord -> [Seat]
adj g c = [ seat | c' <- neighbors c, seat <- maybeToList (g M.!? c') ]

tick :: Grid -> Grid
tick g = M.mapWithKey f g
  where
    f c Empty    | all   (/= Occupied) (adj g c)      = Occupied
    f c Occupied | count (== Occupied) (adj g c) >= 4 = Empty
    f _ seat = seat

part2 :: Grid -> Int
part2 = M.size . M.filter (== Occupied) . dup . iterate tick2

far :: Grid -> Coord -> [Seat]
far g (C y x) = concatMap look dirs
  where
    ray (dy,dx) = tail [ g M.!? C (y+i) (x+j) | i <- [0,dy..] | j <- [0,dx..] ]
    dirs = [(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1)]
    look = take 1 . filter (/= Floor) . catMaybes . takeWhile isJust . ray

tick2 :: Grid -> Grid
tick2 g = M.mapWithKey f g
  where
    f c Empty    | all   (/= Occupied) (far g c)      = Occupied
    f c Occupied | count (== Occupied) (far g c) >= 5 = Empty
    f _ seat = seat
