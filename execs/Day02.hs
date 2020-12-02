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
part1 xs
  = length [ undefined
           | (lo,hi,c,pwd) <- xs
           , let pop = length (filter (c==) pwd)
           , pop >= lo
           , pop <= hi
           ]
part2 xs
  = length [ undefined
           | (lo,hi,c,pwd) <- xs
           , lo <= length pwd && hi <= length pwd
           , let p1 = pwd !! (lo-1)
           , let p2 = pwd !! (hi-1)
           , (p1 == c && p2 /= c) || (p1 /= c && p2 == c)
           ]
