{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
module Main
  ( main
  ) where

import Advent
import Advent.Coord
import Advent.Search
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Debug.Trace

main :: IO ()
main =
  do grid <- getParsed parse 11
     --grid <- getParsedTest parse 11 1
     print (part2 grid)

data Seat = Floor | Empty | Occupied
  deriving (Show,Eq)

parse i = M.fromList $ concat $
  [ [ (C y x,the thing) | (x,thing) <- zip [0..] xs ]
  | (y,xs) <- zip [0..] (lines i) ]
  where
    the '.' = Floor
    the 'L' = Empty
    the '#' = Occupied
    the  _  = undefined

part1 = M.size . M.filter (== Occupied) . dup . iterate tick

type Grid = M.Map Coord Seat

adj g c =
  [ seat | c' <- neighbors c, seat <- maybeToList (g M.!? c') ]

tick :: Grid -> Grid
tick g = M.mapWithKey f g
  where
    f c Empty    | all (/= Occupied) (adj g c) = Occupied
    f c Occupied | count (== Occupied) (adj g c) >= 4 = Empty
    f _ seat = seat

dup (x:y:_) | x == y = x
dup (_:xs) = dup xs

part2 = M.size . M.filter (== Occupied) . dup . iterate tick2

far g (C y x) = concatMap look' dirs
  where
    ray (C dy dx) = [ g M.!? C (y+i) (x+j)
                    | i <- tail [0,dy..]
                    | j <- tail [0,dx..] ]
    look = filter (/= Floor) . catMaybes . takeWhile isJust . ray
    look' = maybeToList . listToMaybe . look
    dirs = [C (-1) 0,C (-1) 1,C 0 1,C 1 1,C 1 0,C 1 (-1),C 0 (-1),C (-1) (-1)]

tick2 :: Grid -> Grid
tick2 g = M.mapWithKey f g
  where
    f c Empty    | all   (/= Occupied) (far g c)      = Occupied
    f c Occupied | count (== Occupied) (far g c) >= 5 = Empty
    f _ seat = seat
