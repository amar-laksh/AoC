module Day1
  ( readCalories,
    countCalories,
    topNTotalCalories,
  )
where

import Control.Monad (when)
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromMaybe, listToMaybe)

type Calories = Int

type CaloriesList = [Int]

type Elf = Int

maximumN :: CaloriesList -> Int -> CaloriesList
maximumN _ 0 = []
maximumN elements n = do
  let currentMaximum = maximum elements
  let restOfElements = filter (< currentMaximum) elements
  currentMaximum : maximumN restOfElements (subtract 1 n)

countCalories :: CaloriesList -> CaloriesList
countCalories [el] = [el]
countCalories [first, second] = [first + second]
countCalories caloriesList = do
  let currentCaloriesList = take (fromMaybe 0 (elemIndex 0 caloriesList)) caloriesList
  let totalCurrentCalories = sum currentCaloriesList
  let restOfCaloriesList = drop (length currentCaloriesList + 1) caloriesList
  totalCurrentCalories : countCalories restOfCaloriesList

topNTotalCalories :: CaloriesList -> Int -> [(Elf, Calories)]
topNTotalCalories caloriesList n = do
  let calorieCountList = countCalories caloriesList
  let maximumCaloriesList = maximumN calorieCountList n
  let calories = [(elf, caloriesCount) | (elf, caloriesCount) <- zip [0 ..] calorieCountList, caloriesCount `elem` maximumCaloriesList]
  sortBy (\(_, a) (_, b) -> compare b a) calories

readCalories :: String -> IO CaloriesList
readCalories filename = do
  lns <- lines <$> readFile filename
  return (map (\x -> if x == "" then 0 else read x) lns)
