module Main
  ( main
  ) where

import Advent
import Data.Complex
import Data.List (foldl')

main :: IO ()
main =
  do i <- getParsedLines parse 12
     print (part1 i)
     print (part2 i)

type C = Complex Double

parse :: String -> (Char,C)
parse (x:(read -> n)) = (x,n:+0)
parse xs = error (show xs)

dir, rot :: Char -> Maybe C

dir 'N' = Just (   0  :+   1  )
dir 'E' = Just (   1  :+   0  )
dir 'S' = Just (   0  :+ (-1) )
dir 'W' = Just ( (-1) :+   0  )
dir  _  = Nothing

rot 'L' = Just (   0  :+   1  )
rot 'R' = Just (   0  :+ (-1) )
rot  _  = Nothing

part1, part2 :: [(Char,C)] -> Int
step1, step2 :: (C,C) -> (Char,C) -> (C,C)

part1 ins = abs (round x) + abs (round y)
  where
    x :+ y = fst $ foldl' step1 ( 0 :+ 0 , 1 :+ 0 ) ins

step1 (p,h) (a,n)
  | Just v <- dir a = ( p + v*n , h             )
  | Just v <- rot a = ( p       , h * v**(n/90) )
  | otherwise       = ( p + h*n , h             )

part2 ins = abs (round x) + abs (round y)
  where
    x :+ y = fst $ foldl' step2 ( 0 :+ 0 , 10 :+ 1 ) ins

step2 (p,w) (a,n)
  | Just v <- dir a = ( p       , w + v*n       )
  | otherwise       = step1 (p,w) (a,n)
