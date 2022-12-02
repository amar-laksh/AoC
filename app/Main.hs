module Main where

import qualified Day1 as T1

main :: IO ()
main = do
  -- Day 1
  caloriesList <- T1.readCalories "/home/amar/github/challenges/aoc/inputs/input1.txt"
  print (T1.mostCalories caloriesList)
  print (T1.topNTotalCalories caloriesList 3)
