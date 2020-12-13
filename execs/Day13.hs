{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main
  ( main
  ) where

import Advent
import Data.List.Split (splitOn)
import Control.Monad (zipWithM)

main :: IO ()
main =
  do i <- getParsed parse 13
     --i <- getParsedTest parse 13 1
     print (part1 i)
     print (part2 i)

parse raw = (read @Int earliest,buses)
  where
    [earliest,rest] = lines raw
    buses = [ (read @Int bus,delay)
            | bus <- splitOn "," rest, bus /= "x"
            | delay <- [0..]
            ]

part1 (earliest,map fst -> ids) = head $
  [ bus*i
  | i <- [0..]
  , let j = earliest + i
  , bus <- ids
  , let r = j `mod` bus
  , r == 0
  ]

part2 (_,needle) = crt residues moduli
  where
    moduli   = [ m   | (m,_) <- needle ]
    residues = [ m-r | (m,r) <- needle ]

-- t      `mod` bus1 == 0
-- t+arr1 `mod` bus2 == 0
-- t+arr3 `mod` bus3 == 0
-- ..
-- t+arrN `mod` busN == 0
-- all at the same time
-- all the busN are prime
-- t mod b1 = 0
-- t mod b2 = -a2 = b2-a2
-- ..
-- t mod nN = -aN = bN-aN

-- chinese remainder theorem code from:
-- https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell

egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b

modInv :: Int -> Int -> Either String Int
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for "
                 ++ show a ++ " and " ++ show b

crt :: [Int] -> [Int] -> Either String Int
crt residues modulii =
  zipWithM modInv crtModulii modulii >>=
    (Right . (`mod` modPI)
           . sum
           . zipWith (*) crtModulii
           . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii
