module Main
  ( main
  ) where

import Advent (getInputLines)
import Numeric (readInt)
import Data.List (sort)

main :: IO ()
main =
  do i <- getInputLines readBinary 5
     print (part1 i)
     print (part2 i)

readBinary x | [(n,_)] <- readInt 2 (`elem` "FBLR") digit x = n
  where
    digit 'F' = 0; digit 'B' = 1
    digit 'L' = 0; digit 'R' = 1

part1, part2 :: [Int] -> Int

part1 = maximum

part2 = gap . sort
  where
    gap (x:y:_) | x+2 == y = x+1
    gap (_:xs) = gap xs
