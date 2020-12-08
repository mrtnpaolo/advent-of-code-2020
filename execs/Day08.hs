{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import Advent
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (findIndices)

main :: IO ()
main =
  do i <- getParsedLines parse 8
     -- i <- map parse . lines <$> getRawTest 8 1
     print (part1 i)
     print `mapM_` part2 i

type Ins = (String,Int)

parse :: String -> Ins
parse (words -> [op,'+':(read -> arg)]) = (op, arg)
parse (words -> [op,'-':(read -> arg)]) = (op,-arg)

part1 :: [Ins] -> Int
part1 = finalAcc . run (-1) . load

data VM = VM
  { _mem  :: IntMap Ins
  , _seen :: IntSet
  , _acc  :: Int
  , _ip   :: Int
  } deriving (Show)

load :: [Ins] -> VM
load xs =
  VM { _mem  = IM.fromList (zip [0..] xs)
     , _seen = IS.empty
     , _acc  = 0
     , _ip   = 0
     }

data Effect = Infinite Int | Terminate Int

finalAcc :: Effect -> Int
finalAcc (Infinite  acc) = acc
finalAcc (Terminate acc) = acc

run :: Int {- ^ index of instruction to flip -} -> VM -> Effect
run ix vm@VM{..}

  -- infinite loop
  | IS.member _ip _seen = Infinite _acc

  -- correct termination
  | _ip == IM.size _mem = Terminate _acc

  -- instruction processing
  | otherwise = run ix $ case instr of

      ("jmp",off) -> ip off .         look $ vm
      ("nop",_  ) -> ip 1   .         look $ vm
      ("acc",n  ) -> ip 1   . acc n . look $ vm

    where
      patch "jmp" = "nop"
      patch "nop" = "jmp"
      patch   x   =   x

      instr = case _mem IM.! _ip of
        (op,arg) | _ip == ix -> (patch op,arg)
                 | otherwise -> (      op,arg)

      look   vm@VM{..} = vm { _seen = IS.insert _ip _seen }
      ip off vm@VM{..} = vm { _ip   = _ip + off }
      acc n  vm@VM{..} = vm { _acc  = _acc + n }

part2 :: [Ins] -> [Int]
part2 mem = [ n | Terminate n <- [ run i (load mem) | i <- indices ] ]
  where
    indices = findIndices ((`elem` ["jmp","nop"]) . fst) mem
