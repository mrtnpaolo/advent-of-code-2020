{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import Advent
import Numeric (readInt)
import Data.List (sort)

main :: IO ()
main =
  do ns <- getParsedLines readBin 5
     print (part1 ns)
     print (part2 ns)
  where
    readBin x | [(n,_)] <- readInt 2 (`elem` "FBLR") digit x = n
    digit 'F' = 0; digit 'L' = 0
    digit 'B' = 1; digit 'R' = 1

part1, part2 :: [Int] -> Int

part1 = maximum

part2 = gap . sort
  where
    gap (x:y:_) | x+2 == y = x+1
    gap (_:xs )            = gap xs
