{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import Advent
import Numeric (readInt)
import Data.List (sort)

type Seat = String

main :: IO ()
main =
  do rows <- lines <$> getRawInput 5
     print (part1 rows)
     print (part2 rows)

part1, part2 :: [Seat] -> Int

part1 = maximum . map seat

seat :: Seat -> Int
seat (splitAt 7 -> (r,c)) = calc (readBin r) (readBin c)
  where
    calc r c = r*8 + c

    readBin x | [(n,_)] <- readInt 2 (`elem` "FBLR") digit x = n

    digit 'F' = 0; digit 'L' = 0
    digit 'B' = 1; digit 'R' = 1

part2 = gap . sort . map seat
  where
    gap (x:y:_) | x+2 == y = x+1
    gap (_:xs )            = gap xs
