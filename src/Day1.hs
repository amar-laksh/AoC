module Day1
  ( day1,
  )
where

import Common (splitOn)
import Data.List (sortBy)

type Calories = Int

type Elf = Int

readCaloriesList :: String -> IO [(Calories, Elf)]
readCaloriesList filename = do
  lst <- map (map (\x -> read x :: Int)) . splitOn "" . lines <$> readFile filename
  return [(elf, sum calories) | (elf, calories) <- zip [0 ..] lst]

topNTotalCalories :: [(Calories, Elf)] -> Int -> [(Calories, Elf)]
topNTotalCalories caloriesList n = do
  let sorted = sortBy (\(_, a) (_, b) -> compare b a) caloriesList
  take n sorted

day1 = do
  print "***Day 1***"
  caloriesList <- readCaloriesList "./inputs/input1.txt"
  let mostCalories = head (topNTotalCalories caloriesList 1)
  let topThreeElfs = map fst (topNTotalCalories caloriesList 3)
  let topThreeTotalCalories = sum (map snd (topNTotalCalories caloriesList 3))
  print ("The elf: " ++ show (fst mostCalories) ++ " has the most calories: " ++ show (snd mostCalories))
  print ("The elfs: " ++ show topThreeElfs ++ " have a combined total of " ++ show topThreeTotalCalories ++ " calories")
