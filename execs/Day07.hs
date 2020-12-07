{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  ( main
  ) where

import Advent
import qualified Data.Map.Strict as M
import Data.List (unfoldr,partition)
import Data.List.Split (splitOn)

main :: IO ()
main =
  do rs <- getParsedLines parseLine 7
     print (part1 rs)

parseLine = match . words . init
  where
    match (c1:c2:"bags":"contain":(concatMap (each . words) . splitOn ", " . unwords -> bags)) =
      (unwords [c1,c2],bags)
    each ["no","other","bags"]      = []
    each ((read @Int -> n):c1:c2:_) = [(n,unwords [c1,c2])]

-- open each bag until either shiny gold or no bags are inside
part1 rs = sum $ takeWhile (>0) $ unfoldr open bags
  where
    rs' = M.fromList [ (c,cs) | (c,map snd -> cs) <- rs ]
    bags = [ cs | (_,map snd -> cs) <- rs ]
    open xs =
      case partition ("shiny gold" `elem`) xs of
        (length -> n,rest) -> Just (n,map (concatMap (rs' M.!)) rest)

{-
-- invert the map
-- then start from "shiny gold" keeping track of all the colors seen
part1 rs = go S.empty (S.singleton "shiny gold")
  where
    inverted = M.fromListWith (++) [ y | (c,map snd -> cs) <- rs
                                       , y <- (\x -> (x,[c])) <$> cs ]
    go seen cs
      | S.null cs = S.size seen
      | otherwise = go seen' cs'
      where
        cs' = S.fromList . concat . M.elems . M.restrictKeys inverted $ cs
        seen' = seen `S.union` cs'
-}
