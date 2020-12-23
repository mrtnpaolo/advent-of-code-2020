{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  ( main
  ) where

import Advent

import Data.Ix
import Data.Ord
import Data.List
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Foldable
import Data.Traversable

import Control.Monad
import Control.Applicative

import Data.Sequence (Seq(..),(><),(<|))
import Data.Sequence      qualified as Seq
import Data.Set           qualified as  S
import Data.IntSet        qualified as IS
import Data.Map.Strict    qualified as  M
import Data.IntMap.Strict qualified as IM

import Data.Array.IArray  qualified as  A

import Data.List.Split

import Debug.Trace

--import Text.Regex.Base
--import Text.Regex.TDFA

import Control.Monad.ST qualified as ST
import Data.Array.ST qualified as ST
import Data.Array.Unboxed qualified as U
import Data.Array.MArray qualified as M
import Data.Array.IArray qualified as A

main :: IO ()
main =
  do print (part1 9 start)
     print (part2 1_000_000 (toList start))

type Cups = Seq Int

start :: Cups
start = Seq.fromList [4,8,7,9,1,2,3,6,5]
--start = Seq.fromList [3,8,9,1,2,5,4,6,7]

-- clockwise rotation by n
cw n s = Seq.drop n $ Seq.cycleTaking (length s + n) s

part1 m = f . (!! 100) . iterate (tick m)

tick m (curr :<| a :<| b :<| c :<| rest) = rotated
  where
    picked =  S.fromList [a,b,c]
    remain = curr :<| rest
    --[next] = take 1 . filter (`S.notMember` picked) . tail . iterate (pred' m) $ curr
    next = until (\n -> n/=a&&n/=b&&n/=c) (pred' m) (pred' m curr)
    (left,_ :<| right) = Seq.spanl (next /=) remain
    new = left >< (next <| a <| b <| c <| right)
    rotated = cw 1 new
tick _ _ = undefined

pred' m x = let y = pred x in y `seq` if y == 0 then m else y

f s = r <> l
  where
    (l,1 :<| r) = Seq.spanl (1 /=) s

--part2 :: Int -> [Int] -> Int
part2 m s = (a A.! 1) * (a A.! (a A.! 1))
-- zipWith (A.!) (repeat a) ([1..12] ++ [999_997,999_998,999_999,1_000_000])
  where
    a :: U.UArray Int Int = ST.runSTUArray $
          do arr <- M.newListArray (1,m) (map snd cups) :: ST.ST s (ST.STUArray s Int Int)
             go 1 arr (head s)
    (lastcup,firstcup) : (reverse -> restcups) = reverse $ zip s (tail (cycle s))
    extra = succ (maximum s)
    cups = sort ((lastcup,extra) : restcups) ++
             zip [extra .. pred m] (map succ [extra .. pred m]) ++
               [(m,firstcup)]

    go :: forall s. Int -> ST.STUArray s Int Int -> Int -> ST.ST s (ST.STUArray s Int Int)
    go 10_000_000 arr _ = pure arr :: ST.ST s (ST.STUArray s Int Int)
    --go 10 arr _ = pure arr :: ST.ST s (ST.STUArray s Int Int)
    go i arr curr =
      do -- pick up cups a,b,c and tie curr to d to re-make the circle
         -- curr,a,b,c,d -> curr,d
         a <- M.readArray arr curr
         b <- M.readArray arr a
         c <- M.readArray arr b
         d <- M.readArray arr c
         M.writeArray arr curr d
         -- calc the destination
         let dest = until (\n -> n/=a&&n/=b&&n/=c) (pred' m) (pred' m curr)
         -- find dest,x and plop a,b,c into them: dest,a,b,c,x
         x <- M.readArray arr dest
         M.writeArray arr dest a
         M.writeArray arr c x
         -- find the next current cup
         next <- M.readArray arr curr
         when (i `mod` 1_000_000 == 0) $
            traceM (show (i,curr,next))
         go (succ i) arr next
