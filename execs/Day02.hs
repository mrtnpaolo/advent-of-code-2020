module Main
  ( main
  ) where

import Advent

main :: IO ()
main =
  do input <- map parse . lines <$> getRawInput 2
     print (part1 input)
     print (part2 input)
  where
    parse = parseLine . words . map replace
    parseLine [lo,hi,c,pwd] = (read lo,read hi,head c,pwd)
    parseLine _ = error "malformed line"
    replace '-' = ' '
    replace ':' = ' '
    replace  x  =  x

part1, part2 :: [(Int,Int,Char,String)] -> Int

part1 = count valid
  where
    valid (lo,hi,c,pwd)
      = let pop = count (c==) pwd
        in lo <= pop && pop <= hi

part2 = count valid
  where
    valid (lo,hi,c,pwd)
      = let p1 = pwd !! (lo-1) == c
            p2 = pwd !! (hi-1) == c
        in p1 /= p2
