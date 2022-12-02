module Task1
  ( readCalories,
    countCalories,
    mostCalories,
    topNTotalCalories,
  )
where

import Control.Monad (when)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, listToMaybe)

maximumN :: [Int] -> Int -> [Int]
maximumN _ 0 = []
maximumN elements n = do
  let currentMaximum = maximum elements
  let restOfElements = filter (< currentMaximum) elements
  currentMaximum : maximumN restOfElements (subtract 1 n)

countCalories :: [Int] -> [Int]
countCalories [el] = [el]
countCalories [first, second] = [first + second]
countCalories caloriesList = do
  let currentCaloriesList = take (fromMaybe 0 (elemIndex 0 caloriesList)) caloriesList
  let totalCurrentCalories = sum currentCaloriesList
  let restOfCaloriesList = drop (length currentCaloriesList + 1) caloriesList
  totalCurrentCalories : countCalories restOfCaloriesList

mostCalories :: [Int] -> (Int, Int)
mostCalories caloriesList = do
  let calorieCountList = countCalories caloriesList
  let maximumCalories = maximum calorieCountList
  let calories = head [(elf, caloriesCount) | (elf, caloriesCount) <- zip [0 ..] calorieCountList, caloriesCount == maximumCalories]
  calories

topNTotalCalories :: [Int] -> Int -> Int
topNTotalCalories caloriesList n = do
  let calorieCountList = countCalories caloriesList
  let maximumCaloriesList = maximumN calorieCountList n
  let calories = [caloriesCount | caloriesCount <- calorieCountList, caloriesCount `elem` maximumCaloriesList]
  sum calories

readCalories :: String -> IO [Int]
readCalories filename = do
  lns <- lines <$> readFile filename
  return (map (\x -> if x == "" then 0 else read x) lns)
