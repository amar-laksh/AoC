module Day3
  ( day3,
  )
where

import Common
import Data.Char (chr, isAsciiLower, ord)
import Data.List (minimumBy, sortBy)

toAsciiScheme c = do
  if isAsciiLower c
    then ord c - 96
    else ord c - 38

fromAsciiScheme c = do
  if c < 26
    then chr (c + 96)
    else chr (c + 38)

readRucksacksInNGroups :: String -> Int -> IO [[String]]
readRucksacksInNGroups filename n = do
  lns <- lines <$> readFile filename
  let newLns = concatMap (takeInN n) [lns]
  return newLns

commonItemType ruckSacksList = do
  let midPoint lst = length lst `div` 2
  let duplicateList lst = get3rd (head (filter (\(lindex, uindex, _) -> lindex < midPoint lst && uindex > midPoint lst) [(indexOf c lst, length lst - indexOf c (reverse lst), c) | (i, c) <- zip [0 ..] lst, count (== c) lst > 1]))
  sum [toAsciiScheme (duplicateList lst) | lst <- ruckSacksList]

-- commonBadgeType ruckSacksList = do

day3 = do
  print "***Day 3***"
  ruckSacksList <- readRucksacksInNGroups "./inputs/input3.demo" 1
  print (commonItemType (concat ruckSacksList))
  ruckSacksList <- readRucksacksInNGroups "./inputs/input3.demo" 3
  print ruckSacksList

-- print (map concat (concatMap (takeInN 3) [ruckSacksList]))

-- print (concatMap (\x -> if indexOf x r `div` 3 == 1 then x else " ") r)
