{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import Advent          (getInput)

import Data.Ix         (range)
import Data.Ord        (comparing)
import Data.List       (transpose,sortBy,isPrefixOf)

import Data.List.Split (splitOn)

import qualified Data.Set as S     (fromList,size,delete)
import qualified Data.IntSet as IS (fromList,member,notMember)

main :: IO ()
main =
  do i <- getInput parse 16
     print (part1 i)
     print (part2 i)

parse (splitOn "\n\n" -> [a,b,c]) = (rules,mine,nearby)
  where
    rules = map rule (lines a)

    rule line = (name,numbers)
      where
        [name,rest]     = splitOn ": " line
        numbers         = IS.fromList (range (lo,hi) ++ range (lo',hi'))
        [lo,hi,lo',hi'] = map (read @Int) (words (map clean rest))
        clean c | c `elem` "-or" = ' '
                | otherwise      =  c

    mine   = map (read @Int) . splitOn "," . (!! 1) . lines $ b

    nearby = map (map (read @Int) . splitOn ",") . tail . lines $ c

invalid rules n = and [ n `IS.notMember` s | (_,s) <- rules ]

part1 (rules,_,nearby) = sum [ n | ticket <- nearby, n <- ticket, invalid rules n ]

part2 (rules,mine,nearby) = product [ value | (value,name) <- sieve ordered
                                            , "departure " `isPrefixOf` name ]
  where
    valid = mine : filter (not . any (invalid rules)) nearby

    candidates = zip mine [ S.fromList [ n | (n,s) <- rules, all (`IS.member` s) ns ]
                          | ns <- transpose valid ]

    ordered = sortBy (comparing (S.size . snd)) candidates

    sieve [] = []
    sieve ((val,[name]):rest) = -- inputs are s.t. this is always the case
      (val,name) : sieve [ (v,S.delete name names) | (v,names) <- rest ]
