module Main
  ( main
  ) where

import Advent    (getInput)
import Data.List (findIndex)

main :: IO ()
main =
  do i <- getInput parse 25
     print (part1 i)

parse = map (read @Int) . lines

part1 [cpk,dpk] = transform cls dpk where Just cls = loopsize cpk
            -- or transform dls cpk where Just dls = loopsize dpk

loopsize n = findIndex (n==) (iterate (\x -> x * 7 `mod` 20201227) 1)

transform ls subject = (iterate (\x -> x * subject `mod` 20201227) 1) !! ls
