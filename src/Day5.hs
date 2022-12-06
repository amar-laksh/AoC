module Day5
  ( day5,
    readStackAndProcedures,
  )
where

import Common (indexOf, replace, splitOn)
import Data.List (maximumBy)

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

emptyPadding padLength = splitOn ',' (take (padLength - 1) $ cycle ",")

addPadding stacks padLength = do
  [addPad stack | stack <- stacks]
  where
    addPad stack
      | length stack < padLength = emptyPadding (padLength - length stack) ++ stack
      | otherwise = stack

applyProcedure :: [Int] -> [[String]] -> [[String]]
applyProcedure procedure stacks = do
  updateStack (cleanStack stacks)
  where
    [totalElementsToRemove, from, to] = procedure
    updateStack stacks = addPadding [addToStack stack stackIdx | (stackIdx, stack) <- zip [1 ..] stacks] (padLength stacks)
    cleanedStack = cleanStack stacks
    padLength stack = length (maximumBy (\x y -> compare (length x) (length y)) stack)
    elementsToAdd = concat [take totalElementsToRemove (filter (/= "") stack) | (stackIdx, stack) <- zip [1 ..] stacks, stackIdx == from]
    removeFromStack stack stackIdx
      | stackIdx == from = emptyPadding totalElementsToRemove ++ drop totalElementsToRemove (filter (/= "") stack)
      | otherwise = stack
    addToStack stack stackIdx
      | stackIdx == to = reverse elementsToAdd ++ filter (/= "") stack
      | otherwise = stack
    cleanStack stacks = addPadding [removeFromStack stack stackIdx | (stackIdx, stack) <- zip [1 ..] stacks] (padLength stacks)

day5 = do
  print "***Day 5***"
  (stacks, procedures) <- readStackAndProcedures "./inputs/input5.txt"
  let lastStack = foldr applyProcedure stacks (reverse procedures)
  print (filter (\x -> x `notElem` ['[', ']']) (concatMap (head . filter (/= "")) lastStack))

-- print (filter (\x -> x `notElem` ['[', ']']) (concatMap head (foldr applyProcedure stacks (reverse procedures))))
