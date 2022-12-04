module Day4
  ( day4,
  )
where

import Common (count, intersect, splitOn, subset)

readSectionAssignments :: String -> IO [[[Int]]]
readSectionAssignments filename = do
  lns <- lines <$> readFile filename
  return $ map (map (\x -> read x :: Int) . splitOn '-') <$> map (splitOn ',') lns

totalPairs :: [[[Int]]] -> ([Int] -> [Int] -> Bool) -> Int
totalPairs sections fn = do
  count (== True) [fn [head f .. last f] [head s .. last s] | [f, s] <- sections]

day4 = do
  print "***Day 4***"
  sections <- readSectionAssignments "./inputs/input4.txt"
  print (totalPairs sections (\x y -> subset x y || subset y x))
  print (totalPairs sections intersect)
