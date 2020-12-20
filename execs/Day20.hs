{-# OPTIONS_GHC -Wno-unused-imports #-}
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
     mapM_ print (part1 i)

parse = map readTile . init . splitOn "\n\n"

readTile xs = (n,cs)
  where
    r x | x `elem` "Tile:" = ' ' | otherwise = x
    (header:rows) = lines xs
    [read @Word64 -> n] = words . map r $ header
    cs = [ (y,x) | (y,cols) <- zip [0..] rows, (x,'#') <- zip [0..] cols ]

part1 ts = nub . sort . map (map _id . corners) $ ps4x4
  where
    ps1x1 = [ (n,perimeter n cs) | (n,cs) <- ts ]
    ps2x2 = zip [0..] (fours 10 ps1x1)
    ps4x4 = (fours 20 ps2x2)

nw t = _nw t >>= _nw
ne t = _ne t >>= _nw
se t = _se t >>= _nw
sw t = _sw t >>= _nw

corners t = map fromJust [ne t,se t,sw t,nw t]

north cs = [ x | (0,x) <- cs ]
east  cs = [ y | (y,9) <- cs ]
south cs = reverse [ x | (9,x) <- cs ]
west  cs = reverse [ y | (y,0) <- cs ]

data T = T
  { _n, _e, _s, _w :: !Word64
  , _id :: !Word64
  , _ne, _se, _sw, _nw :: Maybe T
  } deriving (Show)

clockwise T{..} = [_n,_e,_s,_w]

perimeter tile cs =
  T (toN (north cs))
    (toN (east cs))
    (toN (south cs))
    (toN (west cs))
    tile
    Nothing
    Nothing
    Nothing
    Nothing
--perimeter cs = [ toN x | x <- [north cs,east cs,south cs,west cs] ]

toN :: [Int] -> Word64
toN = foldl' setBit 0

toBits :: Int -> Word64 -> [Bool]
toBits size n = [ testBit n i | i <- [0..size-1] ]

fromBits :: [Bool] -> Word64
fromBits bits = foldl' setBit 0 (findIndices id bits)

--bins ps = length . filter ((>1) . snd) . map (\xs -> (head xs,length xs)) . group $ sort [ x | (_,p) <- ps, x <- p ]

matchingsides ps = M.filter (not . null . tail) $ M.fromListWith (++) [ (x,[n]) | (n,p) <- ps, x <- clockwise p ]

--  4 nw | ne 1
--       A
--  ---D-+-B---  ABCD sides; ne1 se2 sw3 nw4 tiles
--       C
--  3 sw | se 2

--       A   neA = opp A ne

fours size ps =
  [ T n e s w
      (0)
      (Just ne_sides)
      (Just se_sides)
      (Just sw_sides)
      (Just nw_sides)

  | ( side_a , ne_candidates ) <- M.assocs byside
  , ne_id <- ne_candidates

  , let ne_sides = byid M.! ne_id
  , side_b <- good $ place side_a ne_sides

  , se_id <- byside M.! side_b
  , se_id /= ne_id

  , let se_sides = byid M.! se_id
  , side_c <- good $ place side_b se_sides

  , sw_id <- byside M.! side_c
  , sw_id /= ne_id
  , sw_id /= se_id

  , let sw_sides = byid M.! sw_id
  , side_d <- good $ place side_c sw_sides

  , nw_id <- byside M.! side_d
  , nw_id /= ne_id
  , nw_id /= se_id
  , nw_id /= sw_id

  , let nw_sides = byid M.! nw_id
  , side_a `elem` place side_d nw_sides

  -- (T {_n = 307597, _e = 356968, _s = 406828, _w = 602050},2311,1427,2729,1951)

  -- , ne_id == 2311
  -- , se_id == 1427
  -- , sw_id == 2729
  -- , nw_id == 1951

  , let (ne_n,ne_e) = outsides 1 side_a side_b ne_sides
  , let (se_e,se_s) = outsides 2 side_b side_c se_sides
  , let (sw_s,sw_w) = outsides 3 side_c side_d sw_sides
  , let (nw_w,nw_n) = outsides 4 side_d side_a nw_sides

  , let n = fromBits (toBits size nw_n ++ toBits size ne_n)
  , let e = fromBits (toBits size ne_e ++ toBits size se_e)
  , let s = fromBits (toBits size se_s ++ toBits size sw_s)
  , let w = fromBits (toBits size sw_w ++ toBits size nw_w)

  ]
  where
    byside = matchingsides ps
    byid = M.fromList ps
    goodsides = M.keysSet byside
    good = filter (`S.member` goodsides)

place :: Word64 -> T -> [Word64]
place side T{..}
  | _n == side || _s == side = [_e,_w]
  | _w == side || _e == side = [_n,_s]
  | otherwise = undefined

outsides :: Int -> Word64 -> Word64 -> T -> (Word64,Word64)
outsides 1 a b T{..} = outsides 4 a b (T _e _s _w _n 0 Nothing Nothing Nothing Nothing)
outsides 2 b c T{..} = outsides 4 b c (T _s _w _n _e 0 Nothing Nothing Nothing Nothing)
outsides 3 c d T{..} = outsides 4 c d (T _w _n _e _s 0 Nothing Nothing Nothing Nothing)
outsides 4 d a T{..}
  | (d,a) == (_s,_e) = (_w,_n)
  | (d,a) == (_s,_w) = (_e,_n)

  | (d,a) == (_e,_n) = (_s,_w)
  | (d,a) == (_e,_s) = (_n,_w)

  | (d,a) == (_n,_w) = (_e,_s)
  | (d,a) == (_n,_e) = (_w,_s)

  | (d,a) == (_w,_s) = (_n,_e)
  | (d,a) == (_w,_n) = (_s,_e)
