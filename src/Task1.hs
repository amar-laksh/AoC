module Task1
  ( -- ( helloWorldPure,
    --   helloWorldMonadic,
    --   evenWorld,
    readCalories,
    countCalories,
    mostCalories,
  )
where

import Control.Monad (when)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, listToMaybe)

-- evenWorld :: Int -> Bool
-- evenWorld = even
--
-- helloWorldMonadic :: String -> IO ()
-- helloWorldMonadic msg = putStrLn ("hello world: " ++ msg)
--
-- helloWorldPure :: String -> String
-- helloWorldPure msg = "hello world: " ++ msg
--

countCalories :: [Int] -> [Int]
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

readCalories :: String -> IO [Int]
readCalories filename = do
  lns <- lines <$> readFile filename
  return (map (\x -> if x == "" then 0 else read x) lns)
