module Main
  ( main
  ) where

import Advent
import Advent.Coord
import qualified Data.Set as S

type Area = (S.Set Coord,Int,Int)

main :: IO ()
main =
  do area <- parse . lines <$> {- getRawTest 3 1 -} getRawInput 3
     print (part1 area (1,3))
     print (part2 area [(1,1),(1,3),(1,5),(1,7),(2,1)])
  where
    parse rows = (S.fromList ts,width,height)
      where
        ts = [ C y x
             | (y,cols) <- zip [0..] rows
             , (x,item) <- zip [0..] cols
             , item == '#'
             ]
        width  = length (head rows)
        height = length rows

part1 :: Area -> (Int,Int) -> Int
part1 (ts,width,height) (dy,dx) = count (`S.member` ts) cs
  where
    ys = takeWhile (<= height) $ tail [0,dy..]
    xs = take      (length ys) $ tail [0,dx..]
    cs = [ C y (x `mod` width) | y <- ys | x <- xs ]

part2 :: Area -> [(Int,Int)] -> Int
part2 area slopes =
  product [ part1 area slope | slope <- slopes ]
