module Day5
  ( day5,
    readStackAndProcedures,
  )
where

import Common (indexOf, replace, splitOn)
import Control.Monad

readStackAndProcedures :: String -> IO ([[String]], [[Int]])
readStackAndProcedures filename = do
  lns <- lines <$> readFile filename
  let [stacksLns, procedures] = splitOn "" lns
  let numOfStacks = length (words (last stacksLns))
  let stackSymbolWidth = length (head (words (head stacksLns))) + 1
  let cleanProcedures = map (map (\x -> read x :: Int) . filter removeWords . words) procedures
  let cleanStacksLns = map (splitOn ' ' . replace (totalSpaces stackSymbolWidth) " ") (init stacksLns)
  let stacks = filter (/= []) [[stack | stackLn <- cleanStacksLns, (index, stack) <- zip [0 ..] stackLn, index == stackN] | stackN <- [0 .. numOfStacks]]
  return (stacks, cleanProcedures)
  where
    totalSpaces n = take n $ cycle " "
    removeWords word = word `notElem` ["move", "from", "to"]

printStack stacks = do
  sequence_ [putStr (el1 ++ "\t" ++ el2 ++ "\t" ++ el3 ++ "\n") | (el1, el2, el3) <- zip3 (head stacks) (stacks !! 1) (stacks !! 2)]

applyProcedure :: [Int] -> [[String]] -> [[String]]
applyProcedure procedure stacks = do
  let cleanStack = [removeFromStack stack stackIdx | (stackIdx, stack) <- zip [1 ..] stacks]
  [addToStack stack stackIdx | (stackIdx, stack) <- zip [1 ..] cleanStack]
  where
    [totalElementsToRemove, from, to] = procedure
    elementsToAdd = concat [take totalElementsToRemove stack | (stackIdx, stack) <- zip [1 ..] stacks, stackIdx == from]
    removeFromStack stack stackIdx
      | stackIdx == from = drop totalElementsToRemove stack
      | otherwise = stack
    addToStack stack stackIdx
      | stackIdx == to = reverse elementsToAdd ++ filter (/= "") stack
      | otherwise = stack

day5 = do
  print "***Day 5***"
  (stacks, procedures) <- readStackAndProcedures "./inputs/input5.demo"
  print (foldr applyProcedure stacks (reverse procedures))

-- print (filter (\x -> x `notElem` ['[', ']']) (concatMap head (foldr applyProcedure stacks (reverse procedures))))
