module Common (splitOn, indexOf) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

indexOf :: Eq a => a -> [a] -> Int
indexOf delim elements = fromMaybe (length elements) (elemIndex delim elements)

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
