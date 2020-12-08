{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-missing-signatures #-}
module Main
  ( main
  ) where

import Advent
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (findIndices)

main :: IO ()
main =
  do i <- getParsedLines parse 8
     --i <- map parse . lines <$> getRawTest 8 1
     print (part1 i)
     part2 i

type Ins = (String,Int)

parse :: String -> Ins
parse (words -> [ins,'+':(read -> off)]) = (ins, off)
parse (words -> [ins,'-':(read -> off)]) = (ins,-off)

part1 = _acc . run . load

data VM = VM
  { _mem :: IntMap Ins
  , _seen :: Set Int
  , _acc :: Int
  , _ip :: Int
  } deriving (Show)

load xs =
  VM { _mem = IM.fromList (zip [0..] xs)
     , _seen = S.empty
     , _acc = 0
     , _ip = 0
     }

run vm@VM{..}
  | S.member _ip _seen = vm
  | _ip == IM.size _mem = vm
  | otherwise =
    case _mem IM.! _ip of
      ("acc",n)   -> run $ vm { _acc = _acc + n
                              , _ip = _ip + 1
                              , _seen = S.insert (_ip) _seen
                              }
      ("jmp",off) -> run $ vm { _ip = _ip + off
                              , _seen = S.insert (_ip) _seen
                              }
      ("nop",_)   -> run $ vm { _ip = _ip + 1
                              , _seen = S.insert (_ip) _seen
                              }

-- 232 jmps, 73 nops, 305 total
part2 mem = print `mapM_` (map _acc successes)
  where
    jis = findIndices (("jmp"==).fst) mem
    nis = findIndices (("nop"==).fst) mem
    alts =
      [ patch "nop" i | i <- jis ] ++
      [ patch "jmp" i | i <- nis ]
    patch replacement index = rhs ++ (replacement,n) : lhs
      where
        (rhs,(_,n):lhs) = splitAt index mem
    successes =
      filter (\VM{..} -> _ip == IM.size _mem) (map (run . load) alts)
