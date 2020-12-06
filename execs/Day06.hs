module Main
  ( main
  ) where

import Advent
import Data.List (union,intersect)
import Data.List.Split (splitOn)

main :: IO ()
main =
  do i <- getParsed parse 6
     print (part1 i)
     print (part2 i)
  where
    parse = map words . splitOn "\n\n"

part1, part2 :: [[String]] -> Int
part1 = sum . map (length . foldr union [])
part2 = sum . map (length . foldl1 intersect)
