module Main
  ( main
  ) where

import Advent (getInputLines)
import Data.List (tails)

main :: IO ()
main =
  do i <- getInputLines parse 1
     print (part1 i)
     print (part2 i)

parse = read @Int

part1 xs =
  head [ x*y   | x:ys <- tails xs, y <- ys, x+y == 2020 ]

part2 xs =
  head [ x*y*z | x:ys <- tails xs, y:zs <- tails ys, z <- zs, x+y+z == 2020 ]
