{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main
  ( main
  ) where

import Advent
import Data.List (tails,find)
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main =
  do i <- getParsedLines parse 9
     --i <- getParsedTestLines parse 9 1
     --let needle = part1 5 i
     let needle = part1 25 i
     print needle
     print (part2 needle i)

parse = read @Integer


part1 size ns = head ys
  where
    candidates = map (splitAt size) (tails ns)
    ys = [ x | (pre,x:_) <- candidates
           , let sums = S.fromList [ a+b | a:bs <- tails pre, b <- bs ]
           , x `S.notMember` sums ]

part2 needle ns = (cm,cM,cm+cM)
  where
    sums k = [ (minimum ms, maximum ms, sum ms) | ms <- map (take k) (tails ns) ]
    all' = concatMap sums [2..]
    Just (cm,cM,_) = find (\(_,_,s) -> s == needle) all'

