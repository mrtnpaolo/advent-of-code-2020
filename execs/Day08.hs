module Main
  ( main
  ) where

import Advent (getInputLines)

import Data.List (findIndices)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

main :: IO ()
main =
  do i <- getInputLines parse 8
     print (part1 i)
     print `mapM_` part2 i

data Op = ACC | JMP | NOP deriving (Eq)

opcode "acc" = ACC
opcode "jmp" = JMP
opcode "nop" = NOP

type Ins = (Op,Int)

parse :: String -> Ins
parse (words -> [opcode -> op,'+':(read -> arg)]) = (op, arg)
parse (words -> [opcode -> op,'-':(read -> arg)]) = (op,-arg)

part1 :: [Ins] -> Int
part1 = finalAcc . run (-1) . load

data VM = VM
  { _mem  :: IntMap Ins
  , _seen :: IntSet
  , _acc  :: Int
  , _ip   :: Int
  }

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

      (JMP,off) -> ip off .         look $ vm
      (NOP,_  ) -> ip 1   .         look $ vm
      (ACC,n  ) -> ip 1   . acc n . look $ vm

    where
      patch JMP = NOP
      patch NOP = JMP

      instr = case _mem IM.! _ip of
        (op,arg) | _ip == ix -> (patch op,arg)
                 | otherwise ->       (op,arg)

      look     vm@VM{..} = vm { _seen = IS.insert _ip _seen }
      ip   off vm@VM{..} = vm { _ip   = _ip + off           }
      acc  n   vm@VM{..} = vm { _acc  = _acc + n            }

part2 :: [Ins] -> [Int]
part2 mem = [ n | Terminate n <- [ run i (load mem) | i <- ixs ] ]
  where
    ixs = findIndices (`elem` [JMP,NOP]) [ op | (op,_) <- mem ]
