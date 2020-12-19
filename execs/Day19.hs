{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
{-# LANGUAGE FlexibleContexts #-}
module Main
  ( main
  ) where

import Advent

import Data.Ix
import Data.Ord
import Data.List
import Data.Bits
import Data.Char
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

import Text.Regex.Base
import Text.Regex.TDFA

data R
  = Lit Char
  | Conc [Int]
  | Ptr Int
  | Or R R
  deriving (Show)

main :: IO ()
main =
  do i <- getInput parse 19
     print (part1 i)
     print (part2 i)

parse :: String -> (M.Map Int R,[String])
parse raw = (rules,ys)
  where
    [a,b] = splitOn "\n\n" raw
    rules = M.fromList $ map toR $ lines a
    toR xs = r
      where
        [read -> n,rest] = splitOn ": " xs
        r = case words rest of
              ["\"a\""]         -> ( n, Lit 'a')
              ["\"b\""]         -> ( n, Lit 'b')
              [r1]              -> ( n, Ptr (read r1))
              rs@[r1,r2]        -> ( n, Conc (map read rs))
              [r1,"|",r2]       -> ( n, Or (Ptr (read r1))
                                           (Ptr (read r2)))
              rs@[r1,r2,r3]     -> ( n, Conc (map read rs))
              [r1,r2,"|",r3,r4] -> ( n, Or (Conc (map read [r1,r2]))
                                           (Conc (map read [r3,r4])))
              xs -> error (show xs)
    ys = lines b

part1 (rules,strings) = count re strings
  where
    re :: String -> Bool
    re = (=~ concat ["^",expand 0 rules,"$"])

expand :: Int -> M.Map Int R -> String
expand n rs = go (rs M.! n)
  where
    go :: R -> String
    go (Lit c)    = [c]
    go (Conc rs') = concatMap (\i -> go (rs M.! i)) rs'
    go (Ptr p)    = go (rs M.! p)
    go (Or r1 r2) = concat ["(",go r1,"|",go r2,")"]

part2 (rules,strings) = count valid [ (l,r) | Right (l,r) <- map (go 0 0 False) strings ]
  where
    valid (l,r) = r > 0 && l > r

    r42 = concat [ "^", expand 42 rules ]
    r31 = concat [ "^", expand 31 rules ]

    go l r False s
      | [a] <- s =~ r42 :: [MatchText String]
                        = let (_,(0,offset)) = a A.! 0 in go (l+1) r False (drop offset s)
      | otherwise       = go l r True s
    go l r True s
      | null s          = Right (l,r)
      | [a] <- s =~ r31 :: [MatchText String]
                        = let (_,(0,offset)) = a A.! 0 in go l (r+1) True (drop offset s)
      | otherwise       = Left (l,r)

{-
part2 (rules,strings) = map (\s -> eat s t) strings
  where
    t = rules M.! 0

    eat [] _          = Right ""

    eat (c:cs) (Lit x)
      | c == x        = Right cs
      | otherwise     = Left (show (c,cs,"is not Lit",x))

    eat cs (Conc ns)  = foldM eat cs [ rules M.! n | n <- ns ]

    eat cs (Ptr p)    = eat cs (rules M.! p)

    eat cs (Or r1 r2) = eat cs r1 <|> eat cs r2
-}
