module Main
  ( main
  ) where

import Advent          (getInput,select)
import Advent.Search   (dfsOn)

import Data.Ix         (range,inRange)
import Data.List       (transpose,findIndices,isPrefixOf)

import Data.List.Split (splitOn)

main :: IO ()
main =
  do i <- getInput parse 16
     print (part1 i)
     print (part2 i)

parse raw = (rules,mine,nearby)
  where
    [a,b,c] = splitOn "\n\n" raw
    rules = map rule . lines $ a
    rule r = (name,(x,y),(w,z))
      where
        [name,rest] = splitOn ": " r
        [x,y,w,z] = map (read @Int) . words . map rep $ rest
        rep x | x `elem` "-or" = ' ' | otherwise = x
    mine = map (read @Int) $ splitOn "," (lines b !! 1)
    nearby = map (map (read @Int) . splitOn ",") . tail . lines $ c

part1 (rules,_,tickets) = sum $ concat [ invalid t | t <- tickets ]
  where
    allranges = concat [ concat [range r1,range r2] | (_,r1,r2) <- rules ]
    invalid xs = [ x | x <- xs, x `notElem` allranges ]

type Rule = (String,(Int,Int),(Int,Int))

data SS =
  SS { _avail :: [Rule]
     , _found :: [Rule]
     , _preds :: [Rule -> Bool]
     }

part2 (rules,mine,othertickets) = product [ v | v <- map (mine !!) ixs ]
  where
    ixs = findIndices departures (reverse (_found solution))

    departures (name,_,_) = "departure " `isPrefixOf` name

    [solution] = filter (null . _avail) (dfsOn repr next start)

    nearbys    = filter (not . invalid) othertickets
    invalid xs = or [ x `notElem` allranges | x <- xs ]
    allranges  = concat [ concat [range r1,range r2] | (_,r1,r2) <- rules ]

    checks = [ \(_,r1,r2) -> and [ inRange r1 n || inRange r2 n | n <- ns ]
             | ns <- transpose nearbys ]

    start =
      SS { _avail = rules
         , _found = []
         , _preds = checks }

    next SS{..}
      | null _avail = []
      | otherwise =
        [ SS { _avail = remaining
             , _found = rule : _found
             , _preds = ps }
        | (rule,remaining) <- select _avail
        , let (p:ps) = _preds
        , p rule
        ]

    repr SS{..} = _avail
