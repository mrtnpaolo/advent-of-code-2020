{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import Advent
import qualified Data.Map.Strict as M
import Data.List (unfoldr,partition)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)

type Rule = (String,[(Int,String)])

main :: IO ()
main =
  do rs <- getParsedLines parseLine 7
     print (part1 rs)

parseLine :: String -> Rule
parseLine = match . words . init
  where
    match (c1:c2:"bags":"contain":inside) = (unwords [c1,c2],bags)
      where
        bags = concatMap (each . words) . splitOn ", " . unwords $ inside
    each ["no","other","bags"]      = []
    each ((read @Int -> n):c1:c2:_) = [(n,unwords [c1,c2])]

-- open each bag until either shiny gold or no bags are inside
part1 :: [Rule] -> Int
part1 rules = sum $ takeWhile (>0) $ unfoldr (Just . open) (M.elems contain)
  where
    open = bimap length (map openAll) . partition ("shiny gold" `elem`)
    openAll = concatMap (contain M.!)
    contain = M.fromList [ (c,map snd cs) | (c,cs) <- rules ]
