module Main where

import qualified Task1 as T1

main :: IO ()
main = do
  caloriesList <- T1.readCalories "/home/amar/github/challenges/aoc/inputs/input1.txt"
  print (T1.mostCalories caloriesList)
