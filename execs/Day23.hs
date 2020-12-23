{-# LANGUAGE ScopedTypeVariables #-}
module Main
  ( main
  ) where

import Data.List (sort)
import Data.Foldable (toList)

import Data.Sequence (Seq(..),(><),(<|))
import Data.Sequence qualified as Seq

import Control.Monad.ST   (ST)
import Data.Array.ST      qualified as ST (runSTUArray,STUArray)
import Data.Array.Unboxed qualified as U (UArray)
import Data.Array.MArray  qualified as M (newListArray,readArray,writeArray)
import Data.Array.IArray  qualified as A ((!))

main :: IO ()
main =
  do print (read @Int $ concatMap show $ toList $ part1 9 start)
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
    remain = curr :<| rest
    next = until (\n -> n/=a&&n/=b&&n/=c) (pred' m) (pred' m curr)
    (left,_ :<| right) = Seq.spanl (next /=) remain
    new = left >< (next <| a <| b <| c <| right)
    rotated = cw 1 new
tick _ _ = undefined

pred' m x = let y = pred x in y `seq` if y == 0 then m else y

f s = r <> l
  where
    (l,1 :<| r) = Seq.spanl (1 /=) s

part2 m s = (a A.! 1) * (a A.! (a A.! 1))
  where
    a :: U.UArray Int Int = ST.runSTUArray $
          do arr <- M.newListArray (1,m) (map snd cups) :: ST s (ST.STUArray s Int Int)
             go 1 arr (head s)
    (lastcup,firstcup) : (reverse -> restcups) = reverse $ zip s (tail (cycle s))
    extra = succ (maximum s)
    cups = sort ((lastcup,extra) : restcups) ++
             zip [extra .. pred m] (map succ [extra .. pred m]) ++
               [(m,firstcup)]

    go :: forall s. Int -> ST.STUArray s Int Int -> Int -> ST s (ST.STUArray s Int Int)
    go 10_000_000 arr _ = pure arr :: ST s (ST.STUArray s Int Int)
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
         -- find dest,x and plop a,b,c between them: dest,a,b,c,x
         x <- M.readArray arr dest
         M.writeArray arr dest a
         M.writeArray arr c x
         -- find the next current cup
         next <- M.readArray arr curr
         go (succ i) arr next
