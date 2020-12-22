{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import Advent          (getInput)
import Data.List.Split (splitOn)

import Data.Foldable                    (toList)
import Data.Sequence                    (Seq(..),(|>))
import Data.Sequence   qualified as Seq (fromList,null,length,take)
import Data.Set        qualified as Set (empty,member,insert)

main :: IO ()
main =
  do i <- getInput parse 22
     print (part1 i)
     print (part2 i)

parse i = (Seq.fromList p1, Seq.fromList p2)
  where
    [p1,p2] = map (map (read @Int)) . map (tail . lines) . splitOn "\n\n" $ i

part1 = score . winner . until finished combat

combat (x :<| xs,y :<| ys)
  | x > y      = (xs |> x |> y,ys)
  | otherwise  = (xs,ys |> y |> x)

finished (p1,p2) = Seq.null p1 || Seq.null p2

winner (p1,Empty) = p1
winner (Empty,p2) = p2

score = sum . zipWith (*) [1..] . reverse . toList

part2 = score . winner . snd . until (finished . snd) rcombat . (Set.empty,)

rcombat (seen,players)

  -- repeating
  | players `Set.member` seen = (seen,(fst players,Empty))

  -- can play a subgame
  | Seq.length xs >= x && Seq.length ys >= y = case recursive of
      (_,Empty) -> (seen',(xs |> x |> y,ys))
      (Empty,_) -> (seen',(xs,ys |> y |> x))

  -- or the bigger draw wins
  | x > y = (seen',(xs |> x |> y,ys))
  | True  = (seen',(xs,ys |> y |> x))

  where
    (x :<| xs,y :<| ys) = players
    seen' = Set.insert players seen
    recursive = snd $
      until (finished . snd) rcombat (Set.empty,(Seq.take x xs,Seq.take y ys))
