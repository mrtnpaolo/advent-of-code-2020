module Advent.Coord where

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq)

cardinal :: Coord -> [Coord]
cardinal c = c `seq` [above c, right c, below c, left c]

above, right, below, left :: Coord -> Coord
above (C y x) = C (y-1) x
right (C y x) = C y (x+1)
below (C y x) = C (y+1) x
left  (C y x) = C y (x-1)

origin :: Coord
origin = C 0 0

manhattan :: Coord -> Coord -> Int
manhattan (C x y) (C u v) = abs (x-u) + abs (y-v)
