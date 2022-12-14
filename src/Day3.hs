{-# OPTIONS_GHC -Wall #-}

module Day3
  ( day3,
  )
where

import Common (takeInN)
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
  sum [toPriority (uncurry commonItem (halves lst)) | lst <- map concat ruckSacksList]
  where
    halves lst = splitAt (length lst `div` 2) lst
    commonItem fh sh = head [char | char <- fh, char `elem` sh]

sumOfcommonBadge :: [[String]] -> Int
sumOfcommonBadge ruckSacksList = do
  sum [toPriority (commonBadge firstL secondL thirdL) | [firstL, secondL, thirdL] <- ruckSacksList]
  where
    commonBadge fl sl tl = head [char | char <- fl, char `elem` sl, char `elem` tl]

day3 :: IO ()
day3 = do
  print "***Day 3***"
  ruckSacks1 <- readRucksacksInNGroups "./inputs/input3.txt" 1
  print (sumOfItemPriorities ruckSacks1)
  ruckSacks2 <- readRucksacksInNGroups "./inputs/input3.txt" 3
  print (sumOfcommonBadge ruckSacks2)
