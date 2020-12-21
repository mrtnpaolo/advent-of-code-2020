module Main
  ( main
  ) where

import Advent

import Data.List
import Data.List.Split
import Data.Map.Strict qualified as M

main :: IO ()
main =
  do i <- getInput parse 21
     let dangerous = sieve i
     print (part1 (map fst i) (map snd dangerous))
     putStrLn (part2 dangerous)

parse = map g . map f . lines . map r
  where
    r c | c `elem` "()," = ' ' | True = c
    f = map words . splitOn "contains"
    g [ingredients,allergens] = (ingredients,allergens)

part1 ingredients foods = length $ concatMap (\\ foods) ingredients

sieve [] = []
sieve i = (a,food) : sieve rest
  where
    ((a,[food]):_) = M.assocs . M.filter (null . tail) $
      M.fromListWith intersect [ (a,fs) | (fs,als) <- i, a <- als ]
    rest = [ (delete food fs,als')
           | (fs,als) <- i
           , let als' = delete a als
           , not (null als') ]

part2 = intercalate "," . map snd . sort
