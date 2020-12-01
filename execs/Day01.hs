module Main
  ( main
  ) where

import Advent
import Data.List (tails)

main :: IO ()
main =
  do xs <- map (read @Int) . lines <$> {- getRawTest 1 1 -} getRawInput 1
     print (part1 xs)
     print (part2 xs)

part1, part2 :: [Int] -> Int
part1 xs = head [ x*y | x:ys <- tails xs, y <- ys, x+y == 2020 ]
part2 xs = head [ x*y*z | x:ys <- tails xs, y:zs <- tails ys, z <- zs, x+y+z == 2020 ]
