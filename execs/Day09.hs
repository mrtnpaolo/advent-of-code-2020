module Main
  ( main
  ) where

import Advent (getInputLines)

import Data.List (tails)

import qualified Data.IntSet as IS

main :: IO ()
main =
  do i <- getInputLines read 9
     let needle = part1 25 i
     print needle
     print (part2 needle i)

part1, part2 :: Int -> [Int] -> Int

part1 size ns =
  head $ [ x
         | (pre,x:_) <- candidates
         , let sums = IS.fromList [ a+b | a:bs <- tails pre, b <- bs ]
         , x `IS.notMember` sums ]
  where
    candidates = map (splitAt size) (tails ns)

part2 needle ns = head [ lo+hi | (lo,hi,tot) <- all', tot == needle ]
  where
    sums k = [ (minimum ms, maximum ms, sum ms)
             | ms <- map (take k) (tails ns) ]
    all' = concatMap (takeWhile (\(_,_,s) -> s <= needle) . sums) [2..]
