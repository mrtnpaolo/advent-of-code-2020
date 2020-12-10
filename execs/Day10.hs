module Main
  ( main
  ) where

import Advent
import Data.List (sort)
import Data.List.Split (split,keepDelimsR,keepDelimsL,whenElt)

main :: IO ()
main =
  do i <- getParsedLines read 10
     print (part1 i)
     print (part2 i)

part1, part2 :: [Int] -> Int

part1 i = count (3==) diffs * count (1==) diffs
  where
    ns = 0 : sort i ++ [3 + maximum i]
    diffs = zipWith (-) (tail ns) ns

part2 (sort -> i) = product [ ways (length g) | g <- grouping ns ]
  where
    ns = 0 : i ++ [3 + last i]

    grouping xs =
      split (keepDelimsR . keepDelimsL . whenElt $ three) (zip xs (tail xs))

    three (a,b) = b - a == 3

    ways 0 = 1
    ways 1 = 1
    ways 2 = 1
    ways 3 = 2
    ways 4 = 4
    ways 5 = 7
    ways x = error ("unsupported: " ++ show x)
