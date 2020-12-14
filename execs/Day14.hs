{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-signatures #-}
module Main
  ( main
  ) where

import Advent
import Advent.Coord
import Advent.Search

import Data.Bits
import Data.List
import Data.Maybe
import Data.List.Split

import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

main :: IO ()
main =
  do i <- getParsedLines parse 14
     --i <- getParsedTestLines parse 14 1
     --i <- getParsedTestLines parse 14 2
     print (part1 i)
     print (part2 i)

data Ins
  = Set !Int !Int
  | Mask !Int {- zeros: and -} !Int {- ones: or -} [Int] {- floating -}
  deriving (Show)

parse = match . words . map r
  where
    r x | x `elem` "[]=maesk" = ' '
        | otherwise           =  x
    match [read -> addr,read -> n] = Set addr n
    match [reverse -> mask] = Mask (readZeros mask) (readOnes mask) (readF mask)
    match xs = error (show xs)
    readZeros, readOnes :: String -> Int
    readZeros mask = foldl' setBit 0 [ i | (i, c ) <- zip [0..] mask, c /= '0' ]
    readOnes  mask = foldl' setBit 0 [ i | (i,'1') <- zip [0..] mask ]
    readF     mask = [ i | (i,'X') <- zip [0..] mask ]

noMask = Mask (foldl' setBit 0 [0..36]) (0) []

part1 = go IM.empty noMask
  where
    go mem _ [] = sum mem
    go mem m@(Mask z o _) (Set addr n : rest) = go mem' m rest
      where
        mem' = IM.insert addr v mem
        v = (n .&. z) .|. o
    go mem _ (m : rest) = go mem m rest

part2 = go IM.empty noMask
  where
    go mem _ [] = sum mem
    go mem m@(Mask z o f) (Set addr n : rest) = go mem' m rest
      where
        mem'  = foldl' (\oldmap key -> IM.insert key n oldmap) mem addrs
        addr' = foldl' imposeBit (addr .|. o) [ (i,0) | i <- f ]
        addrs = [ toAddr $ zip f (bits y (length f)) | y <- [0..2^(length f) - 1] ]
        toAddr = foldl' imposeBit addr'
    go mem _ (m : rest) = go mem m rest

bits' 0 0 = id
bits' 0 k = bits' 0 (k-1) . (0 :)
bits' n k = let (q,r) = n `divMod` 2 in bits' q (k-1) . (r :)

bits n k = bits' n k []

imposeBit :: Int -> (Int,Int) -> Int
imposeBit a (i,0) = clearBit a i
imposeBit a (i,1) = setBit a i
