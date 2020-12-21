module Main
  ( main
  ) where

import Advent
import Advent.Coord

import Data.List       (foldl',sort)
import Data.Foldable   (for_)
import Data.List.Split (splitOn)
import Control.Monad   (guard)

import Data.Set           qualified as  S
import Data.Map.Strict    qualified as  M

main :: IO ()
main =
  do tiles <- getInput parse 20

     let sz      = 12
         pic     = stitch [ C y x | y <- [0..sz-1], x <- [0..sz-1] ] tiles
         corners = [ fst (pic M.! C y x) | y <- [0,sz-1], x <- [0,sz-1] ]

     print (product corners)

     let image = S.fromList [ C (8*y + dy) (8*x + dx)
                            | (C y x,t) <- M.assocs pic
                            , C dy dx <- removeFrame t ]

     let nessie = readGrid $ unlines [ "                  # "
                                     , "#    ##    ##    ###"
                                     , " #  #  #  #  #  #   " ]

     let without = foldl' elide image [ map (translate dx dy) cs
                                      | dx <- [0..8*sz]
                                      , dy <- [0..8*sz]
                                      , cs <- d8 nessie ]

     print (S.size without)

type Tile = [Coord]

parse :: String -> [(Int,Tile)]
parse = map readTile . init . splitOn "\n\n"
  where
    readTile xs = (n,cs)
      where
        r x | x `elem` "Tile:" = ' ' | otherwise = x
        (header:rows) = lines xs
        [read @Int -> n] = words . map r $ header
        cs = readGrid (unlines rows)

type Picture = M.Map Coord (Int,Tile)

stitch :: [Coord] -> [(Int,Tile)] -> Picture
stitch cs tiles = head (go M.empty cs tiles)
  where
    go pic [] _ = [pic]
    go pic (c:cs) tiles =
      do ((n,t),rest) <- select tiles
         t' <- d8 t
         for_ (pic M.!? above c) $ \(_,ta) ->
           guard (sort [ x | C 9 x <- ta ] == sort [ x | C 0 x <- t' ])
         for_ (pic M.!? left c) $ \(_,tl) ->
           guard (sort [ y | C y 9 <- tl ] == sort [ y | C y 0 <- t' ])
         go (M.insert c (n,t') pic) cs rest

d8 :: Tile -> [Tile]
d8 t = take 4 (iterate rotate t) ++ take 4 (iterate rotate (invert t))

rotate :: Tile -> Tile
rotate t = t2
  where
    t1 = map (\(C y x) -> C x (-y)) t
    xm = abs $ minimum [ x | C _ x <- t1 ]
    t2 = map (\(C y x) -> C y (x+xm)) t1

invert t = t2
  where
    t1 = map (\(C y x) -> C (-y) x) t
    ym = maximum [ abs y | C y _ <- t1 ]
    t2 = map (\(C y x) -> C (y+ym) x) t1

removeFrame (_,p) =
  [ C (y-1) (x-1) | C y x <- p, 0 < y, y < 9, 0 < x, x < 9 ]

translate :: Int -> Int -> Coord -> Coord
translate dy dx (C y x) = C (y + dy) (x + dx)

elide :: S.Set Coord -> [Coord] -> S.Set Coord
elide src (S.fromList -> offending)
  | offending `S.isSubsetOf` src = src `S.difference` offending
  | otherwise                    = src
