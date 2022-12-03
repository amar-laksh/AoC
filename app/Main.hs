module Main where

import qualified Day1 as D1
import qualified Day2 as D2

main :: IO ()
main = do
  print "***Day 1***"
  caloriesList <- D1.readCaloriesList "./inputs/input1.txt"
  let mostCalories = head (D1.topNTotalCalories caloriesList 1)
  let topThreeElfs = map fst (D1.topNTotalCalories caloriesList 3)
  let topThreeTotalCalories = sum (map snd (D1.topNTotalCalories caloriesList 3))
  print ("The elf: " ++ show (fst mostCalories) ++ " has the most calories: " ++ show (snd mostCalories))
  print ("The elfs: " ++ show topThreeElfs ++ " have a combined total of " ++ show topThreeTotalCalories ++ " calories")

  print "\n**Day 2***"
  rounds <- D2.readStratergy "./inputs/input2.txt"
  print (sum (D2.applyStratergy1 (D2.toSchemeBasedList rounds)) == 11063)

-- print (sum (map D2.applyResponseStratergy rounds) == 11063)
-- print (sum (map D2.applyConditionStratergy rounds) == 10349)
