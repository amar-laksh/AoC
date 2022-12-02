module Day1
  ( readCaloriesList,
    topNTotalCalories,
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
