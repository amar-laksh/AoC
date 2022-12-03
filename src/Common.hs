module Common (splitOn, indexOf, count, get3rd, takeInN) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

get3rd (_, _, x) = x

indexOf :: Eq a => a -> [a] -> Int
indexOf delim elements = fromMaybe (length elements) (elemIndex delim elements)

count p xs = sum [1 | x <- xs, p x]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim [el]
  | delim == el = []
  | otherwise = [[el]]
splitOn delim elements = do
  if delim `notElem` elements
    then [elements]
    else do
      let elementsLength = length elements
      let delimIdx = indexOf delim elements
      let currentElements = take (if delimIdx == elementsLength then 0 else delimIdx) elements
      let restOfElements = drop (delimIdx + 1) elements
      currentElements : splitOn delim restOfElements

takeInN :: Eq a => Int -> [a] -> [[a]]
takeInN _ [el] = [[el]]
takeInN _ [] = []
takeInN n elements = do
  let currentElements = take n elements
  let restOfElements = take n (drop n elements)
  [currentElements] ++ [restOfElements] ++ takeInN n (drop n (drop n elements))
