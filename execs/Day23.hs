{-# OPTIONS_GHC -Wno-unused-imports #-}
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

import Data.Hashable

main :: IO ()
main =
  do --print (part1 9 start)
     print (part2 1_000_000 start)

--parse = id

type Cups = Seq Int

start :: Cups
start = Seq.fromList [4,8,7,9,1,2,3,6,5]
--start = Seq.fromList [3,8,9,1,2,5,4,6,7]

-- clockwise rotation by n
cw n s = Seq.drop n $ Seq.cycleTaking (length s + n) s

part1 m = f . (!! 100) . iterate (tick m)

tick m (curr :<| a :<| b :<| c :<| rest) = {-traceShow (l,r) -}{- traceShowId -} rotated -- curr :<| rest
  where
    picked =  S.fromList [a,b,c]
    remain = curr :<| rest
    [next] = take 1 . filter (`S.notMember` picked) . tail . iterate (pred' m) $ curr

    (left,_ :<| right) = Seq.spanl (next /=) remain
    new = left >< (next <| a <| b <| c <| right)

    --Just i = {-traceShowId $-} Seq.findIndexL (next ==) remain
    --(left,right) = Seq.splitAt (i+1) remain
    --new = traceShow ("curr",curr,"picked",[a,b,c],"i",i,"l",left,"r",right) $
    --new = left >< (a <| b <| c <| right)

    rotated = cw 1 new
    --oneAt = Seq.findIndexL (1==) rotated
    (Seq.take 3 -> l,Seq.take 3 -> r) = Seq.spanl (1 /=) rotated
tick _ _ = undefined

pred' m x = let y = pred x in y `seq` if y == 0 then m else y

f s = r <> l
  where
    (l,1 :<| r) = Seq.spanl (1 /=) s

part2 m s = go 0 IS.empty (s >< Seq.fromList [10..1_000_000])
  where
    go i seen s
      | IS.member h seen
      , let (l,r) = Seq.spanl (1 /=) s
      = (i,Seq.take 3 l, Seq.take 3 r)
      | otherwise = go (i+1) (IS.insert h seen) (tick m s)
      where
        h = hash (toList s)

