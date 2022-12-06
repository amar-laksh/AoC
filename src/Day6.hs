module Day6
  ( day6,
  )
where

import Common (isSet, zipAll)

readDataStream :: String -> IO [String]
readDataStream filename = lines <$> readFile filename

detectMarker :: Int -> String -> Int
detectMarker markerWidth dataStream = head [last (map fst tuples) + 1 | tuples <- zipAll markersList, isSet (map snd tuples)]
  where
    markersList = [markerTuple n | n <- [0 .. markerWidth - 1]]
    markerTuple n = zip [n ..] (drop n dataStream)

day6 = do
  print "***Day 6***"
  dataStreams <- readDataStream "./inputs/input6.demo"
  print (map (detectMarker 4) dataStreams)
  print (map (detectMarker 14) dataStreams)

  dataStreams <- readDataStream "./inputs/input6.txt"
  print (map (detectMarker 4) dataStreams)
  print (map (detectMarker 14) dataStreams)
