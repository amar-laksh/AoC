module Day3
  ( day3,
  )
where

import Common (count, get3rd, indexOf, takeInN)
import Data.Char (isAsciiLower, ord)

toPriority :: Char -> Int
toPriority c
  | isAsciiLower c = ord c - 96
  | otherwise = ord c - 38

readRucksacksInNGroups :: String -> Int -> IO [[String]]
readRucksacksInNGroups filename n = do
  lns <- lines <$> readFile filename
  return $ concatMap (takeInN n) [lns]

sumOfItemPriorities :: [[String]] -> Int
sumOfItemPriorities ruckSacksList = do
  sum [toPriority (uncurry commonBadge (halves lst)) | lst <- map concat ruckSacksList]
  where
    halves lst = splitAt (length lst `div` 2) lst
    commonBadge fh sh = head [char | char <- fh, char `elem` sh]

sumOfcommonBadge :: [[String]] -> Int
sumOfcommonBadge ruckSacksList = do
  sum [toPriority (commonBadge firstL secondL thirdL) | [firstL, secondL, thirdL] <- ruckSacksList]
  where
    commonBadge fl sl tl = head [char | char <- fl, char `elem` sl, char `elem` tl]

day3 = do
  print "***Day 3***"
  ruckSacksList <- readRucksacksInNGroups "./inputs/input3.txt" 1
  print (sumOfItemPriorities ruckSacksList)
  ruckSacksList <- readRucksacksInNGroups "./inputs/input3.txt" 3
  print (sumOfcommonBadge ruckSacksList)
