{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
module Main
  ( main
  ) where

import Advent
import Advent.Search
import Data.List
import Data.List.Split
--import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Debug.Trace

main :: IO ()
main =
  do i <- getParsedLines parse 10
     --i <- getParsedTestLines parse 10 2
     --print `mapM_` i
     print (part1 i)
     print (part2 i)

parse = read @Int

data S =
  S { from :: Int
    , avail :: IS.IntSet
    , d1, d2, d3 :: Int
    } deriving (Show)

repr S{..} = IS.toAscList avail

{-
next S{..} =
  [ S { from = i
      , avail = IS.delete i avail
      , d1 = if i-from == 1 then d1 + 1 else d1
      , d2 = if i-from == 2 then d2 + 1 else d2
      , d3 = if i-from == 3 then d3 + 1 else d3
      }
  | i <- IS.toList avail, 1 <= i-from, i-from <= 3 ]
-}

next S{..} =
  [ S { from = next
      , avail = IS.delete next avail
      , d1 = if dj == 1 then d1 + 1 else d1
      , d2 = if dj == 2 then d2 + 1 else d2
      , d3 = if dj == 3 then d3 + 1 else d3
      }
  | dj <- [1,2,3], let next = from+dj, next `IS.member` avail ]

part1 ns = d1 s * d3 s
  where
    extra = 3 + maximum ns
    ns' = IS.insert extra (IS.fromList ns)
    start = S { from = 0
              , avail = ns'
              , d1 = 0, d2 = 0, d3 = 0
              }
    Just s = find (IS.null . avail) $ dfsOn repr next start

part2 (sort -> i) = product (map (f . length) (grouping ns))
  where
    ns = 0 : i ++ [3 + last i]

    grouping xs =
      split (keepDelimsR . keepDelimsL . whenElt $ three) (zip xs (tail xs))

    three (a,b) = b - a == 3

    f 0 = 1
    f 1 = 1
    f 2 = 1
    f 3 = 2
    f 4 = 4
    f 5 = 7

{-
data S2 =
  S2 { from2 :: Int
     , avail2 :: IS.IntSet
     , path2 :: [Int]
     } deriving (Show)

part2 raw = (:[]) . length . filter ((0 `IS.notMember`) . avail2) $ dfsOn repr next start
  where
    ns = take (length raw `div` 3) (sort raw)
    end = 3 + last ns
    repr S2{..} = (path2,IS.toList avail2)
    next S2{..} =
      [ S2 { from2 = k
           , avail2 = let (less,_) = IS.split k avail2 in less
           , path2 = k:path2
           }
      | k <- filter (`IS.member` avail2) [from2 - 1, from2 - 2, from2 - 3]
      ]
    start =
      S2 { from2 = end
         , avail2 = IS.fromAscList (0:ns)
         , path2 = [end]
         }
-}

{-
part2 ns = [length xs]
  where
    extra = 3 + maximum ns
    ns' = IS.insert extra (IS.fromList ns)
    start = S { from = 0
              , avail = ns'
              , d1 = 0, d2 = 0, d3 = 0
              }
    xs = filter (IS.notMember extra . avail) $ dfsOn repr next start
-}



{-
dfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
dfsOn rep next start
 -}

{-
part1 ns = go 0 (S.fromList ns) -- jolt 1 * jolt 3
  where
    jolt n = []
    hi = maximum ns
    extra = 3 + hi
    outlet = 0
    possible n = S.toList [ j | j <- ns, 1 <= n-j, n-j <= 3 ]
    go k available = 
      where
        valid = available S.intersection (possible k)

    dfsOn  valid (S.fromList ns,d1,d2,d3)
-}
