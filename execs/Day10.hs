{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main
  ( main
  ) where

import Advent
import Advent.Search (dfsOn)
import Data.List (sort,find)
import Data.List.Split (split,keepDelimsR,keepDelimsL,whenElt)
import qualified Data.IntSet as IS

main :: IO ()
main =
  do i <- getParsedLines parse 10
     --i <- getParsedTestLines parse 10 1
     print (part1 i)
     print (part2 i)

parse = read @Int

data S =
  S { from  :: Int
    , avail :: IS.IntSet
    , d1, d2, d3 :: Int
    } deriving (Show)

repr S{..} = IS.toAscList avail

next S{..} =
  [ S { from  = next
      , avail = IS.delete next avail
      , d1 = if dj == 1 then d1 + 1 else d1
      , d2 = if dj == 2 then d2 + 1 else d2
      , d3 = if dj == 3 then d3 + 1 else d3
      }
  | dj <- [1,2,3], let next = from+dj, next `IS.member` avail ]

part1, part2 :: [Int] -> Int

part1 ns = d1 s * d3 s
  where
    extra = 3 + maximum ns
    ns' = IS.insert extra (IS.fromList ns)
    start = S { from = 0
              , avail = ns'
              , d1 = 0, d2 = 0, d3 = 0
              }
    Just s = find (IS.null . avail) $ dfsOn repr next start

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
