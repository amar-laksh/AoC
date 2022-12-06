{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Day6
  ( day6,
  )
where

import Common (indexOf, isSet, takeInN, zipAll)
import Data.List (zip4)
import Data.Maybe (fromMaybe, listToMaybe)

readDataStream :: String -> IO [String]
readDataStream filename = lines <$> readFile filename

uniqueMarker [] = False
uniqueMarker [c1, c2, c3, c4, c5, c6, c7]
  | isSet [c1, c2, c3, c4, c5, c6, c7] = True
  | otherwise = False
uniqueMarker [c1, c2, c3, c4]
  | isSet [c1, c2, c3, c4] = True
  | otherwise = False
uniqueMarker elements = False

detect4CharMarker :: String -> Int
detect4CharMarker dataStream = head [last (map fst lst) + 1 | lst <- zipAll [pairs 0, pairs 1, pairs 2, pairs 3], uniqueMarker (map snd lst)]
  where
    pairs n = zip [n ..] (drop n dataStream)

day6 = do
  print "***Day 6***"
  dataStreams <- readDataStream "./inputs/input6.demo"
  print (map detect4CharMarker dataStreams == [7, 5, 6, 10, 11])

  dataStreams <- readDataStream "./inputs/input6.txt"
  print (map detect4CharMarker dataStreams == [1623])
