{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  ( main
  ) where

import Advent
import Advent.Coord

import Data.Ix
import Data.Ord
import Data.List
import Data.Bits (testBit,setBit)
import Data.Char
import Data.Word
import Data.Maybe
import Data.Foldable
import Data.Traversable

import Control.Monad
import Control.Applicative

import Data.Set           qualified as  S
import Data.IntSet        qualified as IS
import Data.Map.Strict    qualified as  M
import Data.IntMap.Strict qualified as IM

import Data.Array.IArray  qualified as  A

import Data.List.Split

import Debug.Trace

--import Text.Regex.Base
--import Text.Regex.TDFA

main :: IO ()
main =
  do tiles <- getInput parse 20
     let sz = 3
         pic = head $ stitch M.empty [ C y x | y <- [0..sz-1], x <- [0..sz-1] ] tiles
         corners = map (fst . (pic M.!)) [ C 0 0, C 0 (sz-1), C (sz-1) (sz-1), C (sz-1) 0 ]
     print (product corners)
     --for_ [0..sz-1] $ \y ->
     -- for_ [0..sz-1] $ \x ->
     --   do print (fst (pic M.! C y x))
     --      putStrLn . showGrid . S.toList . snd . (pic M.!) $ C y x
     let image = S.fromList $ [ C (8*y + dy) (8*x + dx)
                              | (C y x,t) <- M.assocs pic
                              , C dy dx <- removeFrame t
                              ]
     for_ (d8 image) $ \i' ->
       putStrLn . showGrid . S.toList $ i'


type Tile = S.Set Coord

parse :: String -> [(Int,Tile)]
parse = map readTile . init . splitOn "\n\n"
  where
    readTile xs = (n,cs)
      where
        r x | x `elem` "Tile:" = ' ' | otherwise = x
        (header:rows) = lines xs
        [read @Int -> n] = words . map r $ header
        cs = S.fromList $ [ C y x
                          | (y,cols) <- zip [0..] rows
                          , (x,'#')  <- zip [0..] cols ]

type Picture = M.Map Coord (Int,Tile)

stitch :: Picture -> [Coord] -> [(Int,Tile)] -> [Picture]
stitch pic []     _     = [pic]
stitch pic (c:cs) tiles =
  do ((n,t),rest) <- select tiles
     t' <- d8 t
     for_ (pic M.!? above c) $ \(_,ta) ->
       guard (sort [ x | C 9 x <- S.toList ta ] == sort [ x | C 0 x <- S.toList t' ])
     for_ (pic M.!? left c) $ \(_,tl) ->
       guard (sort [ y | C y 9 <- S.toList tl ] == sort [ y | C y 0 <- S.toList t' ])
     stitch (M.insert c (n,t') pic) cs rest

d8 :: Tile -> [Tile]
d8 t = take 4 (iterate rotate t) ++ take 4 (iterate rotate (invert t))

rotate :: Tile -> Tile
rotate t = t2
  where
    t1 = S.map (\(C y x) -> C x (-y)) t
    xm = abs $ minimum [ x | C _ x <- S.toList t1 ]
    t2 = S.map (\(C y x) -> C y (x+xm)) t1

invert t = t2
  where
    t1 = S.map (\(C y x) -> C (-y) x) t
    ym = maximum [ abs y | C y _ <- S.toList t1 ]
    t2 = S.map (\(C y x) -> C (y+ym) x) t1

removeFrame (_,p) =
  [ C (y-1) (x-1) | C y x <- S.toList p, 0 < y, y < 9, 0 < x, x < 9 ]
