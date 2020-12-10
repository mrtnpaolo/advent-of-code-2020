{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main
  ( main
  ) where

import Advent
import Data.List (tails)
import qualified Data.Set as S

main :: IO ()
main =
  do i <- getParsedLines read 9
     --i <- getParsedTestLines read 9 1
     --let needle = part1 5 i
     let needle = part1 25 i
     print needle
     print (part2 needle i)

part1, part2 :: Int -> [Int] -> Int

part1 size ns = head ys
  where
    candidates = map (splitAt size) (tails ns)
    ys = [ x | (pre,x:_) <- candidates
           , let sums = S.fromList [ a+b | a:bs <- tails pre, b <- bs ]
           , x `S.notMember` sums ]

part2 needle ns = head [ lo+hi | (lo,hi,tot) <- all', tot == needle ]
  where
    sums k = [ (minimum ms, maximum ms, sum ms)
             | ms <- map (take k) (tails ns) ]
    all' = concatMap (takeWhile (\(_,_,s) -> s <= needle) . sums) [2..]
