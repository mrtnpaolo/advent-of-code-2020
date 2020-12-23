{-# LANGUAGE RankNTypes #-}
module Main
  ( main
  ) where

import Advent

import Data.Char     (isDigit)
import Data.List     (sort)
import Data.Foldable (toList)

import Data.Sequence (Seq(..),(><),(<|))
import Data.Sequence qualified as Seq

import Control.Monad.ST                   (ST)
import Data.Array.ST      qualified as ST (runSTUArray,STUArray)
import Data.Array.MArray  qualified as M  (newListArray,readArray,writeArray)
import Data.Array.IArray  qualified as A  ((!))

main :: IO ()
main =
  do i <- getInput parse 23
     putStrLn (part1 9 i)
     print (part2 1_000_000 (toList i))

parse xs = Seq.fromList [ read @Int [x] | x <- xs, isDigit x ]

-- clockwise rotation by n
cw n s = Seq.drop n $ Seq.cycleTaking (length s + n) s

part1 m = concatMap show . following1 . (!! 100) . iterate (move m)

move m (curr :<| a :<| b :<| c :<| rest) =
  cw 1 (left >< (next <| a <| b <| c <| right))
  where
    remain = curr :<| rest
    next = until (\n -> n/=a && n/=b && n/=c) (pred' m) (pred' m curr)
    (left,_ :<| right) = Seq.spanl (next /=) remain
move _ _ = error "never happens"

-- predecessor, wrapping 0 at m
pred' m x = let y = pred x in y `seq` if y == 0 then m else y

following1 s = r >< l
  where
    (l,1 :<| r) = Seq.spanl (1 /=) s

part2 m s = (a A.! 1) * (a A.! (a A.! 1))
  where
    -- array where each value is the cup to its right (indices are 1 ... 1_000_000)
    -- e.g. 389125467 becomes [2,5,8,6,4,7,10,9,1,11,12,13, .... ,999999,1000000,3]
    a  = ST.runSTUArray $ do arr <- M.newListArray (1,m) (map snd cups)
                             go 1 arr (head s)

    extra = succ (maximum s)
    (lastcup,firstcup) : (reverse -> restcups) = reverse $ zip s (tail (cycle s))
    cups = sort ((lastcup,extra) : restcups) ++
             zip [extra .. pred m] (map succ [extra .. pred m]) ++
               [(m,firstcup)]

    go :: forall s. Int -> ST.STUArray s Int Int -> Int -> ST s (ST.STUArray s Int Int)
    go 10_000_000 arr _ = pure arr
    go i arr curr =
      do -- shorten circle from curr,a,b,c,d to curr,d
         a <- M.readArray arr curr
         b <- M.readArray arr a
         c <- M.readArray arr b
         d <- M.readArray arr c
         M.writeArray arr curr d
         -- figure out the destination
         let dest = until (\n -> n/=a && n/=b && n/=c) (pred' m) (pred' m curr)
         -- plop a,b,c inside dest,x as dest,a,b,c,x
         x <- M.readArray arr dest
         M.writeArray arr dest a
         M.writeArray arr c x
         -- figure out next "current cup"
         next <- M.readArray arr curr
         go (succ i) arr next
