{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main
  ( main
  ) where

import Advent
import Data.Maybe
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

main :: IO ()
main =
  do i <- M.fromList <$> getParsedLines parseLine 7
     print (part1 i)

t = M.fromList . map parseLine . lines <$> getRawTest 7 1

parseLine = f . init . words

f (c1:c2:"bags":"contain":rest) = (unwords [c1,c2],deps)
  where
    deps = concatMap (g . words) . splitOn ", " . unwords $ rest
    g ((read @Int -> n):c1:c2:_) = [(unwords [c1,c2],n)]
    g ["no","other"] = []

part1 = count hasShinyGold . M.toList . dup . iterate walk

hasShinyGold (_,xs) = "shiny gold" `elem` map fst xs

type Rules = Map String [(String,Int)]

walk :: Rules -> Rules
walk rs = M.map (concatMap expand) rs
  where
    expand p@("shiny gold",_) = [p]
    expand (c,n) =
      [ (c',n*m)
      | xs <- maybeToList (rs M.!? c)
      , (c',m) <- xs
      ]

dup (x:y:z:_) | y == z = x
dup (_:xs) = dup xs
