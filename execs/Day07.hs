{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main
  ( main
  ) where

import Advent
import Data.Maybe
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Debug.Trace

main :: IO ()
main =
  do i <- M.fromList <$> getParsedLines parseLine 7
     print (part1 i)
     print (part2 i)

i = M.fromList . map parseLine . lines <$> getRawInput 7
t = M.fromList . map parseLine . lines <$> getRawTest 7 1
t2 = M.fromList . map parseLine . lines <$> getRawTest 7 2

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

part2 rs = go 1 "shiny gold"
  where
    go 0 _ = 0
    go 1 "shiny gold" = sum $ traceShowId [ go n c | (c,n) <- rs M.! "shiny gold" ]
    go want color = want + (sum $ traceShowId [ want * go n c | (c,n) <- bags ])
      where
        bags =
          case rs M.! color of
            [] -> [(undefined,0)]
            xs -> xs

-- 1 sg
-- (1 + 1*do) + (2 + 2*vp)
-- (1 + 1*(3 + 3*fb + 4 + 4*db)) + (2 + 2*(5 + 5*fb + 6 + 6*db))
-- (1 + 1*(3 + 3*0 + 4 + 4*0)) + (2 + 2(5 + 5*0 + 6 + 6*0))
-- (1 + 3 + 4)

