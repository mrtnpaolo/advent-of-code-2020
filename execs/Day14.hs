module Main
  ( main
  ) where

import Advent (getInputLines)

import Data.Bits (setBit,clearBit,(.&.),(.|.))
import Data.List (foldl')

import qualified Data.IntMap.Strict as IM

import Control.Monad (filterM)

main :: IO ()
main =
  do i <- getInputLines parse 14
     print (part1 i)
     print (part2 i)

data Ins
  = Set !Int !Int
  | Mask !Int {- zeros -} !Int {- ones -} [Int] {- floating -}

parse = match . words . map r
  where
    r x | x `elem` "[]=maesk" = ' '
        | otherwise           =  x
    match [read -> addr,read -> n] = Set addr n
    match [reverse -> mask] = Mask (zeros mask) (ones mask) (floating mask)
    match xs = error (show xs)
    zeros    mask = foldl' setBit 0 [ i | (i, c ) <- zip [0..] mask, c /= '0' ]
    ones     mask = foldl' setBit 0 [ i | (i,'1') <- zip [0..] mask ]
    floating mask = [ i | (i,'X') <- zip [0..] mask ]

noMask = Mask (foldl' setBit 0 [0..36]) (0) ([])

part1 = go IM.empty noMask
  where
    go mem m (Set addr n : rest) = go mem' m rest
      where
        Mask z o _ = m
        mem'       = IM.insert addr n' mem
        n'         = (n .&. z) .|. o

    go mem _ (m : rest) = go mem m rest

    go mem _ [] = sum mem

part2 = go IM.empty noMask
  where
    go mem m (Set addr n : rest) = go mem' m rest
      where
        Mask _ o f = m
        mem'       = foldl' insert mem addrs
        insert     = \prev at -> IM.insert at n prev
        addrs      = [ foldl' setBit addr' js | js <- subsets f ]
        addr'      = foldl' clearBit (addr .|. o) f

    go mem _ (m : rest) = go mem m rest

    go mem _ [] = sum mem

subsets = filterM (const [True,False])
