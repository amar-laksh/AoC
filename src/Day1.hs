{-# OPTIONS_GHC -Wall #-}

module Day1
  ( day1,
  )
where

import Common (splitOn)
import Data.List (sortBy)

readCaloriesList :: String -> IO [(Int, Int)]
readCaloriesList filename = do
  lst <- map (map (\x -> read x :: Int)) . splitOn "" . lines <$> readFile filename
  return [(elf, sum calories) | (elf, calories) <- zip [0 ..] lst]

topNTotalCalories :: [(Int, Int)] -> Int -> [(Int, Int)]
topNTotalCalories caloriesList n = do
  take n sorted
  where
    sorted = sortBy (\(_, a) (_, b) -> compare b a) caloriesList

day1 :: IO ()
day1 = do
  print "***Day 1***"
  caloriesList <- readCaloriesList "./inputs/input1.txt"
  let mostCalories = head (topNTotalCalories caloriesList 1)
  let topThreeElfs = map fst (topNTotalCalories caloriesList 3)
  let topThreeTotalCalories = sum (map snd (topNTotalCalories caloriesList 3))
  print ("The elf: " ++ show (fst mostCalories) ++ " has the most calories: " ++ show (snd mostCalories))
  print ("The elfs: " ++ show topThreeElfs ++ " have a combined total of " ++ show topThreeTotalCalories ++ " calories")
