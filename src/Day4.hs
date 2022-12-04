module Day4
  ( day4,
  )
where

import Common (count, intersect, splitOn, subset)

readSectionAssignments :: String -> IO [[[Int]]]
readSectionAssignments filename = do
  lns <- lines <$> readFile filename
  return $ map (map (\x -> read x :: Int) . splitOn '-') <$> map (splitOn ',') lns

totalCompletelyOverlappingPairs :: [[[Int]]] -> Int
totalCompletelyOverlappingPairs sections = do
  count (/= []) [checkSubset firstS secondS | [firstS, secondS] <- sections]
  where
    checkSubset firstS secondS = [firstS | subset [head firstS .. last firstS] [head secondS .. last secondS] || subset [head secondS .. last secondS] [head firstS .. last firstS]]

totalOverlappingPairs :: [[[Int]]] -> Int
totalOverlappingPairs sections = do
  count (/= []) [checIntersection firstS secondS | [firstS, secondS] <- sections]
  where
    checIntersection firstS secondS = [firstS | [head firstS .. last firstS] `intersect` [head secondS .. last secondS]]

day4 = do
  print "***Day 4***"
  sections <- readSectionAssignments "./inputs/input4.txt"
  print (totalCompletelyOverlappingPairs sections)
  print (totalOverlappingPairs sections)
