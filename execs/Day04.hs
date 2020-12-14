module Main
  ( main
  ) where

import Advent (getInput,count,ri)
import Data.Char (isDigit)
import Data.List.Split (splitOn)

type Passport = [(String,String)]

main :: IO ()
main =
  do i <- getInput parse 4
     print (part1 i)
     print (part2 i)

parse :: String -> [Passport]
parse = map passport . splitOn "\n\n"
  where
    passport            = map field . words
    field (a:b:c:':':v) = (a:b:c:[],v)
    field xs            = error (show xs)

part1, part2 :: [Passport] -> Int

part1 = count valid

valid :: Passport -> Bool
valid (map fst -> keys) = all (`elem` keys) required
  where
    required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part2 = count (all (uncurry strict)) . filter valid

strict :: String -> String -> Bool

strict "byr" (ri -> n) = 1920 <= n && n <= 2002
strict "iyr" (ri -> n) = 2010 <= n && n <= 2020
strict "eyr" (ri -> n) = 2020 <= n && n <= 2030

strict "hgt" (reverse -> xs)
  | ('m':'c':(ri . reverse -> n)) <- xs = 150 <= n && n <= 193
  | ('n':'i':(ri . reverse -> n)) <- xs =  59 <= n && n <=  76
  | otherwise = False

strict "hcl" ('#':xs) = length xs == 6 && all (`elem` "0123456789abcdef") xs

strict "ecl" xs = xs `elem` ["amb","blu","brn","gry","grn","hzl","oth"]

strict "pid" xs = length xs == 9 && all isDigit xs

strict "cid" _  = True

strict _     _  = False
