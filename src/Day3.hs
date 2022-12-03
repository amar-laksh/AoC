module Day3
  ( day3,
  )
where

import Common (count, get3rd, indexOf, takeInN)
import Data.Char (chr, isAsciiLower, ord)
import Data.List (minimumBy, sortBy)

toPriority c = do
  if isAsciiLower c
    then ord c - 96
    else ord c - 38

readRucksacksInNGroups :: String -> Int -> IO [[String]]
readRucksacksInNGroups filename n = do
  lns <- lines <$> readFile filename
  let newLns = concatMap (takeInN n) [lns]
  return newLns

sumOfItemPriorities :: [[Char]] -> Int
sumOfItemPriorities ruckSacksList = do
  let midPoint lst = length lst `div` 2
  let withinBound lb ub p = lb < p && ub > p
  let commonItems lst = get3rd (head (filter (\(lb, ub, _) -> withinBound lb ub (midPoint lst)) [(indexOf c lst, length lst - indexOf c (reverse lst), c) | (i, c) <- zip [0 ..] lst, count (== c) lst > 1]))
  sum [toPriority (commonItems lst) | lst <- ruckSacksList]

findCommonBadge :: String -> String -> String -> Char
findCommonBadge firstRuckSack secondRuckSack thirdRuckSack = do
  head [char | char <- firstRuckSack, char `elem` secondRuckSack, char `elem` thirdRuckSack]

commonBadgeType :: [[String]] -> Int
commonBadgeType ruckSacksList = do
  sum [toPriority (findCommonBadge (head lst) (lst !! 1) (lst !! 2)) | lst <- ruckSacksList]

day3 = do
  print "***Day 3***"
  ruckSacksList <- readRucksacksInNGroups "./inputs/input3.txt" 1
  print (sumOfItemPriorities (concat ruckSacksList))
  ruckSacksList <- readRucksacksInNGroups "./inputs/input3.txt" 3
  print (commonBadgeType ruckSacksList)
