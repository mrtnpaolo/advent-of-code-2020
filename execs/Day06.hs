module Main
  ( main
  ) where

import Advent
import Data.List (sort,nub,foldl1',intersect)
import Data.List.Split (splitOn)

main :: IO ()
main =
  do i <- getParsed parse 6
     print (part1 i)
     print (part2 i)

  where
    parse = map words . splitOn "\n\n"

part1, part2 :: [[String]] -> Int

part1 = sum . map (length . nub . sort . concat)

part2 = sum . map (length . nub . foldl1' intersect)
