module Main
  ( main
  ) where

import Advent (getInputLines,count)

main :: IO ()
main =
  do i <- getInputLines parse 2
     print (part1 i)
     print (part2 i)

parse = line . words . map r
  where
    line [read -> lo,read -> hi,head -> c,pwd] = (lo,hi,c,pwd)
    line xs = error (show xs)
    r '-' = ' '
    r ':' = ' '
    r  x  =  x

part1 = count valid1

valid1 (lo,hi,c,pwd) = lo <= pop && pop <= hi
  where
    pop = count (c==) pwd

part2 = count valid2

valid2 (lo,hi,c,pwd) = p1 /= p2
  where
    p1 = pwd !! (lo-1) == c
    p2 = pwd !! (hi-1) == c
