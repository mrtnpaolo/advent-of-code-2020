module Advent.Coord where

import Data.Ix

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq, Ix)

cardinal :: Coord -> [Coord]
cardinal c = c `seq` [above c, right c, below c, left c]

above, right, below, left :: Coord -> Coord
above (C y x) = C (y-1) x
right (C y x) = C y (x+1)
below (C y x) = C (y+1) x
left  (C y x) = C y (x-1)

neighbors :: Coord -> [Coord]
neighbors c = c `seq` [above c, left c, right c, below c,
                       above (left c), above (right c),
                       below (left c), below (right c)]

origin :: Coord
origin = C 0 0

manhattan :: Coord -> Coord -> Int
manhattan (C x y) (C u v) = abs (x-u) + abs (y-v)

showGrid :: [Coord] -> String
showGrid g = unlines $
  [ [ if C y x `elem` g
      then '#'
      else '.'
    | x <- [xm..xM] ]
  | y <- [ym..yM] ]
  where
    ys = [ y | C y _ <- g ]
    xs = [ x | C _ x <- g ]

    (ym,yM) = (minimum ys,maximum ys)
    (xm,xM) = (minimum xs,maximum xs)

readGrid :: String -> [Coord]
readGrid raw =
  [ C y x | (y,rows) <- zip [0..] (lines raw)
          , (x,'#' ) <- zip [0..] rows ]
