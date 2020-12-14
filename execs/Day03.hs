module Main
  ( main
  ) where

import Advent (getInput,count)
import Advent.Coord (Coord(..))

import qualified Data.Array.Unboxed as A

type Area = A.UArray Coord Char

main :: IO ()
main =
  do area <- getInput parse 3
     print (part1 area (1,3))
     print (part2 area [(1,1),(1,3),(1,5),(1,7),(2,1)])

parse :: String -> Area
parse (lines -> rows) = A.array (C 0 0, C (h-1) (w-1)) $
  [ (C y x,c) | (y,cols) <- zip [0..] rows, (x,c) <- zip [0..] cols ]
  where
    h = length rows
    w = length (head rows)

part1 :: Area -> (Int,Int) -> Int
part1 area (dy,dx) = count ('#'==) [ area A.! c | c <- path ]
  where
    path = [ C y (x `mod` (xM+1)) | y <- ys | x <- xs ]
    (_,C yM xM) = A.bounds area
    ys = takeWhile (<=yM) [0,dy..]
    xs = take (length ys) [0,dx..]

part2 :: Area -> [(Int,Int)] -> Int
part2 area slopes = product [ part1 area slope | slope <- slopes ]
