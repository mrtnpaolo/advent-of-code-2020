module Main
  ( main
  ) where

import Advent (getInput)

import Data.List.Split (splitOn)

import qualified Data.IntMap.Strict as IM

main :: IO ()
main =
  do i <- getInput parse 15
     print (part1 i)
     print (part2 i)

parse = map (read @Int) . splitOn ","

part1 = (!! (2020 - 1)) . play

part2 = (!! (30000000 - 1)) . play

play ns = game
  where
    game = ns ++ go (IM.fromList $ zip ns [ [i] | i <- [1..] ]) (1 + length ns) (last ns)

    go !seen !i !lastnumber
      = n `seq` n : go (IM.insertWith upd n [i] seen) (i+1) n
        where
          n = case seen IM.!? lastnumber of
                Just turns | length turns > 1 -> let (t1:t0:_) = turns in t1-t0
                _ -> 0

-- upd new old
upd [new] [] = [new]
upd [new] [t] = [new,t]
upd [new] (t:_) = [new,t]
