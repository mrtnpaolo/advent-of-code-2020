{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main
  ( main
  ) where

import Advent
--import Advent.Coord
import Advent.Search

import Data.Ix
import Data.List
import Data.Maybe

import Data.List.Split

import Data.Set qualified as S
import Data.Set qualified as IS
import Data.Map.Strict qualified as M
import Data.IntMap.Strict qualified as IM

--import Data.Array qualified as A
--import Data.Array.Unboxed qualified as A
--import Data.Array.IArray qualified as A
--import Data.Array.MArray qualified as A

data Coord = C !Int !Int !Int !Int
  deriving (Read, Show, Ord, Eq, Ix)

neighbors :: Coord -> [Coord]
neighbors c@(C w z y x) =
  c `seq` [ C (w+dw) (z+dz) (y+dy) (x+dx)
          | dx <- [-1,0,1], dy <- [-1,0,1], dz <- [-1,0,1], dw <- [-1,0,1]
          , abs dx + abs dy + abs dz + abs dw > 0 ]

origin :: Coord
origin = C 0 0 0 0


data Cube = On | Off deriving (Show,Eq)

main :: IO ()
main =
  do i <- getInput parse 17
     print (part1 i)

parse :: String -> M.Map Coord Cube
parse raw = M.fromList $
  [ (C 0 0 y x,b) | (y,row) <- zip [0..] (lines raw)
                , (x,a) <- zip [0..] row
                , let b = case a of '.' -> Off; '#' -> On ]

part1 = M.size . (!! 6) . iterate tick

--tick m = M.mapWithKey (f m) m
tick :: M.Map Coord Cube -> M.Map Coord Cube
tick m = M.filter (On ==) . M.fromList $
  [ (c,f m' c x) | c <- cs, x <- case (m M.!? c) of Nothing -> [Off]; Just s -> [s] ]
  where
    m' = M.filter (On ==) m
    cs = hull (M.keys m')


f m c On | actives == 2 || actives == 3 = On
         | otherwise = Off
  where actives = length (catMaybes [ m M.!? c' | c' <- neighbors c])
f m c Off | actives == 3 = On
          | otherwise    = Off
  where actives = length (catMaybes [ m M.!? c' | c' <- neighbors c])

hull :: [Coord] -> [Coord]
hull cs = S.toList $ S.fromList $ cs ++ concat [ neighbors c | c <- cs ]
