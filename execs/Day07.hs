module Main
  ( main
  ) where

import Advent (getInputLines)
import Data.List (partition)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

main :: IO ()
main =
  do i <- getInputLines parse 7
     print (part1 i)
     print (part2 i)

parse line = (bag,bags)
  where
    [bag,inside] = splitOn " bags contain " line
    bags = concatMap (readBag . words) (splitOn ", " (init inside))
    readBag ["no","other","bags"] = []
    readBag [read @Int -> n,c1,c2,_] = [(n,unwords [c1,c2])]
    readBag xs = error (show xs)

part1 rules = go 0 (M.elems rs)
  where
    rs = M.fromList [ (c,map snd cs) | (c,cs) <- rules ]
    openAll = concatMap (rs M.!)

    go n bags
      | 0 == golds = n
      | otherwise  = go (n+golds) others
      where
        (length -> golds,map openAll -> others) =
          partition ("shiny gold" `elem`) bags

part2 rules = pred (go 1 "shiny gold")
  where
    rs = M.fromList rules

    go 0    _     = 0
    go want color = want + want * sum [ go n c | (n,c) <- rs M.! color ]
