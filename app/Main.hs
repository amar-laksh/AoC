module Main where

import qualified Day1 as D1
import qualified Day2 as D2

main :: IO ()
main = do
  -- Day 1
  caloriesList <- D1.readCalories "/home/amar/github/challenges/aoc/inputs/input1.txt"
  stratergy <- D2.readStratergy "/home/amar/github/challenges/aoc/inputs/input2.txt"
  print (D1.topNTotalCalories caloriesList 1)
  print (D1.topNTotalCalories caloriesList 3)
  print (sum (map D2.outcome stratergy) == 11063)
  print (sum (map D2.newOutcome stratergy) == 10349)

-- Day 2
-- print (D2.0)
