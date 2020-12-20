{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  ( main
  ) where

import Advent

import Data.Ix
import Data.Ord
import Data.List
import Data.Bits
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
  do i <- getInput parse 20
     print (part1 i)

parse = M.fromList . map (\(n,cs) -> (n,toT n cs)) . map readTile . init . splitOn "\n\n"
  where
    readTile xs = (n,cs)
      where
        r x | x `elem` "Tile:" = ' ' | otherwise = x
        (header:rows) = lines xs
        [read @Int -> n] = words . map r $ header
        cs = M.fromList @(Int,Int) $
               [ ((y,x),c)
               | (y,cols) <- zip [0..] rows
               , (x,c)    <- zip [0..] cols ]

    toT n cs = T n (north cs) (east cs) (south cs) (west cs)
      where
        north cs = [ cs M.! (0,x) == '#' | x <- [0..9] ]
        east  cs = [ cs M.! (y,9) == '#' | y <- [0..9] ]
        south cs = [ cs M.! (9,x) == '#' | x <- [9,8..0] ]
        west  cs = [ cs M.! (y,0) == '#' | y <- [9,8..0] ]

data T = T
  { _id            :: Int
  , _n, _e, _s, _w :: [Bool]
  } deriving (Show,Eq,Ord)

data D8 = R0 | R90 | R180 | R270 | H | V | D1 | D2 deriving (Show)

syms T{..} =
  [ {-( R0   ,-} T _id _n _e _s _w
  , {-( R90  ,-} T _id _w _n _e _s
  , {-( R180 ,-} T _id _s _w _n _e
  , {-( R270 ,-} T _id _e _s _w _n
  , {-( H    ,-} T _id (reverse _s) (reverse _e) (reverse _n) (reverse _w)
  , {-( V    ,-} T _id (reverse _n) (reverse _w) (reverse _s) (reverse _e)
  , {-( D1   ,-} T _id (reverse _w) (reverse _s) (reverse _e) (reverse _n)
  , {-( D2   ,-} T _id (reverse _e) (reverse _n) (reverse _w) (reverse _s)
  ]

clockwise T{..} = [_n,_e,_s,_w]

part1 tiles = product corners
  where
    t = tiles M.! 2311

    allsides = S.fromList . map fromBits . concatMap clockwise . concatMap syms $ tiles

    tilesBySide = M.fromList [ (side,tilesWith side) | side <- S.toList allsides ]

    tilesWith side =
      [ _id t
      | t <- M.elems tiles
      , side `elem` [ fromBits bs | bs <- concatMap clockwise (syms t) ]
      ]

    corners =
      [ _id t | t <- M.elems tiles
      , (2==) $ count id
          [ [_id t] == tilesBySide M.! fromBits side | side <- clockwise t ] ]







{-

perimeter n cs = T n (north cs) (east cs) (south cs) (west cs)

toN :: [Int] -> Word64
toN = foldl' setBit 0
-}
toBits :: Int -> Word64 -> [Bool]
toBits size n = [ testBit n i | i <- [0..size-1] ]

fromBits :: [Bool] -> Word64
fromBits bits = foldl' setBit 0 (findIndices id bits)

