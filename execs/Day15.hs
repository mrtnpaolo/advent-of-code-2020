module Main
  ( main
  ) where

import Advent          (getInput)
import Data.Int        (Int32)
import Data.List.Split (splitOn)
import Control.Monad   (zipWithM_,when,unless)
import Text.Printf     (printf)
import Debug.Trace     (traceM)

import Control.Monad.ST  qualified as ST (ST)
import Data.Array.ST     qualified as ST (STUArray,runSTUArray)
import Data.Array.MArray qualified as A  (newArray,writeArray,readArray)

main :: IO ()
main =
  do start <- getInput (map (read @Int32) . splitOn ",") 15
     ST.runSTUArray (play start) `seq` pure ()

play :: [Int32] -> ST.ST s (ST.STUArray s Int32 Int32)
play start =
  do arr <- A.newArray (0,30_000_000) (0)
     zipWithM_ (A.writeArray arr) (init start) [1..]
     loop arr (1 + fromIntegral (length start)) (last start)
     return arr

loop arr i last_n =
  do t0 <- A.readArray arr last_n
     let t1 = i-1
     let n | t0 == 0 = 0 | otherwise = t1-t0
     A.writeArray arr last_n t1
     when   (2020 == i)     $ traceM (printf "2020: %d" n)
     when   (30_000_000 == i) $ traceM (printf "30000000: %d" n)
     unless (30_000_000 == i) $ loop arr (i+1) n
