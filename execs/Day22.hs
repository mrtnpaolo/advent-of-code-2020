{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main
  ( main
  ) where

import Advent

import Data.Ix
import Data.Ord
import Data.List
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Foldable
import Data.Traversable

import Control.Monad
import Control.Applicative

import Data.Sequence
import Data.Sequence      qualified as  Seq
import Data.Set           qualified as  S
import Data.IntSet        qualified as IS
import Data.Map.Strict    qualified as  M
import Data.IntMap.Strict qualified as IM

import Data.Array.IArray  qualified as  A

import Data.List.Split

import Debug.Trace

--import Text.Regex.Base
--import Text.Regex.TDFA

main :: IO ()
main =
  do i <- getInput parse 22
     print (part1 i)
     print (part2 i)

parse i = (Seq.fromList p1, Seq.fromList p2)
  where
    [p1,p2] = map (map (read @Int)) . map (tail . lines) . splitOn "\n\n" $ i

part1 = score . winner . head . dropWhile noWinner . {- Data.List.take 30 . -} iterate tick

tick (p1,p2) = (p1',p2')
  where
    (x :<| xs) = p1
    (y :<| ys) = p2
    p1' | x > y = xs |> x |> y
        | True  = xs
    p2' | y > x = ys |> y |> x
        | True  = ys

noWinner (_ :<| _,_ :<| _) = True
noWinner _ = False

winner (Empty,p2) = p2
winner (p1,Empty) = p1

score = sum . Data.List.zipWith (*) [1..] . Data.List.reverse . toList

part2 ps = score . winner . head . dropWhile noWinner . map snd . iterate ({-traceShowId . -}rtick) $ (S.empty,{-traceShowId -}ps)

rtick (history,players@(p1,p2))
  | players `S.member` history = (history,(p1,Empty))
  | otherwise =
    if Seq.length xs >= x && Seq.length ys >= y
      then let p1' = Seq.take x xs
               p2' = Seq.take y ys
               ps' = (p1',p2')
               result = head . dropWhile noWinner . map snd . iterate rtick $ (history,ps')
           in {-traceShow ("subgame",ps') $-} case result of
                (_,Empty) -> (history',(xs |> x |> y,ys))
                (Empty,_) -> (history',(xs,ys |> y |> x))
      else if x > y then (history',(xs |> x |> y,ys))
                    else (history',(xs,ys |> y |> x))
    where
      (x :<| xs) = p1
      (y :<| ys) = p2
      history' = S.insert players history

opptick (p1,p2) = (p1',p2')
  where
    (x :<| xs) = p1
    (y :<| ys) = p2
    p1' | x < y = xs |> x |> y
        | True  = xs
    p2' | y < x = ys |> y |> x
        | True  = ys

score2 (p1,_) (_,Empty) = score p1
score2 (_,p2) (Empty,_) = score p2

