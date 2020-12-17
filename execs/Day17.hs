module Main
  ( main
  ) where

import Advent  (getInput)

import Data.Set (Set)
import Data.Set qualified as S

data C3 = C3 !Int !Int !Int
  deriving (Read, Show, Ord, Eq)

data C4 = C4 !Int !Int !Int !Int
  deriving (Read, Show, Ord, Eq)

class Ord c => Space c where
  neighbors :: c -> [c]

instance Space C3 where
  neighbors c@(C3 z y x) =
    c `seq` [ C3 (z+dz) (y+dy) (x+dx)
            | dx <- [-1,0,1], dy <- [-1,0,1], dz <- [-1,0,1]
            , abs dx + abs dy + abs dz > 0 ]

instance Space C4 where
  neighbors c@(C4 w z y x) =
    c `seq` [ C4 (w+dw) (z+dz) (y+dy) (x+dx)
            | dx <- [-1,0,1], dy <- [-1,0,1], dz <- [-1,0,1], dw <- [-1,0,1]
            , abs dx + abs dy + abs dz + abs dw > 0 ]

main :: IO ()
main =
  do i <- getInput parse 17
     print (part1 i)
     print (part2 i)

parse :: String -> [(Int,Int)]
parse (lines -> rows) =
  [ (y,x) | (y,cols) <- zip [0..] rows
          , (x,'#' ) <- zip [0..] cols ]

part1 = solve . map (\(y,x) -> C3 0 y x)

part2 = solve . map (\(y,x) -> C4 0 0 y x)

solve :: Space c => [c] -> Int
solve = S.size . (!! 6) . iterate tick . S.fromList

tick :: Space c => Set c -> Set c
tick w = S.filter (live w) . hull $ w

live :: Space c => Set c -> c -> Bool
live w c
  | c `S.member` w = 3 == actives || 2 == actives
  | otherwise      = 3 == actives
  where
    actives = S.size (S.intersection w (S.fromList (neighbors c)))

hull :: Space c => Set c -> Set c
hull cs = S.unions (cs : [ S.fromList (neighbors c) | c <- S.toList cs ])
