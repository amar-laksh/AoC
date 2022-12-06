module Day5
  ( day5,
    readStackAndProcedures,
  )
where

import Common (replace, splitOn)
import Data.List (maximumBy)

readStackAndProcedures :: String -> IO ([[String]], [[Int]])
readStackAndProcedures filename = do
  lns <- lines <$> readFile filename
  let [stacksLns, procedures] = splitOn "" lns
  let stackSymbolWidth = length (head (words (head stacksLns))) + 1
  let cleanProcedures = map (map (\x -> read x :: Int) . filter (\x -> x `notElem` ["move", "from", "to"]) . words) procedures
  let removeDedupSpaces = map (splitOn ' ' . replace (totalSpaces stackSymbolWidth) " ") (init stacksLns)
  let stacks = filter (/= []) [[stack | stackLn <- removeDedupSpaces, (index, stack) <- zip [0 ..] stackLn, index == stackN] | stackN <- [0 .. length (words (last stacksLns))]]
  return (stacks, cleanProcedures)
  where
    totalSpaces n = take n $ cycle " "

emptyPadding padLength = replicate padLength ""

addPadding stacks padLength = do
  [addPad stack | stack <- stacks]
  where
    addPad stack
      | length stack < padLength = emptyPadding (padLength - length stack) ++ stack
      | otherwise = stack

stackAddingCrane stack stackIdx elementsToAdd toStack
  | stackIdx == toStack = reverse elementsToAdd ++ filter (/= "") stack
  | otherwise = stack

stackAddingCrane2 stack stackIdx elementsToAdd toStack
  | stackIdx == toStack = elementsToAdd ++ filter (/= "") stack
  | otherwise = stack

applyProcedure :: [Int] -> [[String]] -> ([String] -> Int -> [String] -> Int -> [String]) -> [[String]]
applyProcedure procedure stacks fn = do
  updateStack (cleanStack stacks)
  where
    [totalElementsToRemove, fromStack, toStack] = procedure
    maxStackLength stack = length (maximum stack)
    updateStack stacks = addPadding [fn stack stackIdx elementsToAdd toStack | (stackIdx, stack) <- zip [1 ..] stacks] (maxStackLength stacks)
    elementsToAdd = concat [take totalElementsToRemove (filter (/= "") stack) | (stackIdx, stack) <- zip [1 ..] stacks, stackIdx == fromStack]
    removeFromStack stack stackIdx
      | stackIdx == fromStack = emptyPadding totalElementsToRemove ++ drop totalElementsToRemove (filter (/= "") stack)
      | otherwise = stack
    cleanStack stacks = addPadding [removeFromStack stack stackIdx | (stackIdx, stack) <- zip [1 ..] stacks] (maxStackLength stacks)

day5 = do
  print "***Day 5***"
  (stacks, procedures) <- readStackAndProcedures "./inputs/input5.txt"
  let lastStack = foldr (\x y -> applyProcedure x y stackAddingCrane) stacks (reverse procedures)
  let lastStack2 = foldr (\x y -> applyProcedure x y stackAddingCrane2) stacks (reverse procedures)

  print (filter (\x -> x `notElem` ['[', ']']) (concatMap (head . filter (/= "")) lastStack) == "VCTFTJQCG")
  print (filter (\x -> x `notElem` ['[', ']']) (concatMap (head . filter (/= "")) lastStack2) == "GCFGLDNJZ")
