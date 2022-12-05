module Day5
  ( day5,
    readStackAndProcedures,
  )
where

import Common (indexOf, replace, splitOn)

-- readStackAndProcedures :: String -> IO [[[Int]]]
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

day5 = do
  print "***Day 5***"
  stacks <- readStackAndProcedures "./inputs/input5.demo"
  print stacks
