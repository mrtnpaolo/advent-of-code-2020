module Main
  ( main
  ) where

import Advent
import Data.List.Split (splitOn)
import Data.Char (isDigit)

type Passport = [(String,String)]

main :: IO ()
main =
  do x <- parse <$> getRawInput 4
     print (part1 x)
     print (part2 x)
  where
    parse = map (map parsePair . words) . splitOn "\n\n"
    parsePair (a:b:c:':':value) = (a:b:c:[],value)
    parsePair _ = undefined

part1 :: [Passport] -> Int
part1 = count valid1

valid1 :: Passport -> Bool
valid1 passport = all (`elem` (map fst passport)) expected
  where
    expected = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

part2 :: [Passport] -> Int
part2 = count valid2 . filter valid1

valid2 :: Passport -> Bool
valid2 passport = all (uncurry validVal) passport

ri :: String -> Int
ri = read

validVal :: String -> String -> Bool
validVal "byr" (ri -> n) = 1920 <= n && n <= 2002
validVal "iyr" (ri -> n) = 2010 <= n && n <= 2020
validVal "eyr" (ri -> n) = 2020 <= n && n <= 2030
validVal "hgt" (reverse -> v)
  | ('n':'i':(ri . reverse -> h)) <- v =  59 <= h && h <=  76
  | ('m':'c':(ri . reverse -> h)) <- v = 150 <= h && h <= 193
  | otherwise                          = False
validVal "hcl" ('#':xs) = length xs == 6 && all (`elem` "0123456789abcdef") xs
validVal "hcl" _        = False
validVal "ecl" ecl = ecl `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
validVal "pid" pid = length pid == 9 && all isDigit pid
validVal "cid" _   = True
validVal _     _   = False
