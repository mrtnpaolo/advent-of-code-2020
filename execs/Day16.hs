{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main
  ( main
  ) where

import Advent
import Advent.Search

import Data.Maybe
import Data.List

import Data.List.Split

import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import Debug.Trace

import Data.Ix

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
  SS { _freerules :: [Rule]
     , _foundrules :: [Rule]
     , _ns :: [[Int]]
     }
  deriving (Show)

part2 (rules,mine,othertickets) = product [ v | v <- map (mine !!) ixs ]
  where
    ixs = findIndices (("departure "==) . take (length "departure ") . rname) (reverse $ _foundrules sol)

    rname (rn,_,_) = rn

    --sol = SS {_freerules = [], _foundrules = [("departure location",(34,724),(735,974)),("zone",(35,766),(784,952)),("wagon",(37,797),(810,973)),("departure station",(40,521),(534,950)),("arrival station",(36,536),(552,972)),("route",(34,309),(318,965)),("departure track",(37,258),(268,964)),("duration",(50,920),(929,950)),("departure date",(32,650),(665,964)),("departure platform",(40,329),(353,973)),("train",(47,746),(754,960)),("arrival location",(42,431),(447,952)),("type",(32,406),(423,963)),("departure time",(39,373),(398,950)),("arrival track",(49,836),(852,952)),("price",(35,853),(870,973)),("row",(42,267),(292,962)),("arrival platform",(45,666),(678,952)),("seat",(46,632),(642,954)),("class",(35,600),(623,953))], _ns = []}

    [sol] = filter ((goal==) . length . _foundrules) search
    goal = length mine

    nearbys = filter (not . invalid) othertickets
    allranges = concat [ concat [range r1,range r2] | (_,r1,r2) <- rules ]
    invalid xs = or [ x `notElem` allranges | x <- xs ]

    checks = transpose nearbys

    acceptable (_,r1,r2) nums = and [ inRange r1 n || inRange r2 n | n <- nums ]

    start =
      SS { _freerules = rules
         , _foundrules = []
         , _ns = checks
         }

    next SS{..}
      | null _freerules = []
      | otherwise =
        [ SS { _freerules = _freerules \\ [rule]
             , _foundrules = rule : _foundrules
             , _ns = rest
             }
        | rule <- _freerules
        , let (ns:rest) = _ns
        , acceptable rule ns
        ]

    repr SS{..} = _freerules

    search = dfsOn repr next start

{-
part2 (rules,mine,othertickets) = fields
  where
    allranges = concat [ concat [range r1,range r2] | (_,r1,r2) <- rules ]
    invalid xs = or [ x `notElem` allranges | x <- xs ]

    nearbys = filter (not . invalid) othertickets

    complete fs = length fs == length mine

    fields = filter complete $ bfsOn repr next start

    start = []

    next fields =
      [ fields ++ [rname]
      | (rname,r1,r2) <- freerules
      , all (\field -> inRange r1 field || inRange r2 field)
            [ t !! length fields | t <- nearbys ]
      ]
      where
        freerules = [ r | r@(rname,_,_) <- rules, rname `notElem` fields ]

    repr = id
-}

-- dfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
-- dfsOn rep next start = loop Set.empty [start]
