module Main
  ( main
  ) where

import Advent    (getInput)
import Data.List (findIndex)

main :: IO ()
main =
  do i <- getInput parse 25
     --let i = (5764801,17807724)
     print (part1 i)

parse = (\[cpk,dpk] -> (cpk,dpk)) . map (read @Int) . lines

part1 (cpk,dpk) = (transform cls dpk,transform dls cpk)
  where
    (Just cls,Just dls) = (loopsize cpk,loopsize dpk)

loopsize n = findIndex (n==) (iterate (\x -> x * 7 `mod` 20201227) 1)

transform ls subject = (iterate (\x -> x * subject `mod` 20201227) 1) !! ls
