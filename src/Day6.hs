{-# OPTIONS_GHC -Wall #-}

module Day6
  ( day6,
  )
where

import Common (isSet, zipAll)

readDataStream :: String -> IO [String]
readDataStream filename = lines <$> readFile filename

detectMarker :: Int -> String -> Int
detectMarker markerWidth dataStream = head [getLastIndex tuples | tuples <- zipAll chunksList, isSet (getChunk tuples)]
  where
    getLastIndex tuples = last (map fst tuples) + 1
    getChunk = map snd
    chunksList = [chunkTuple n | n <- [0 .. markerWidth - 1]]
    chunkTuple n = zip [n ..] (drop n dataStream)

day6 :: IO ()
day6 = do
  print "***Day 6***"
  dataStreams1 <- readDataStream "./inputs/input6.demo"
  print (map (detectMarker 4) dataStreams1)
  print (map (detectMarker 14) dataStreams1)

  dataStreams2 <- readDataStream "./inputs/input6.txt"
  print (map (detectMarker 4) dataStreams2)
  print (map (detectMarker 14) dataStreams2)
