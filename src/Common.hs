module Common (splitOn) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- This is the helper but I decided to create mine
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim [el]
  | delim == el = []
  | otherwise = [[el]]
splitOn delim elements = do
  if delim `notElem` elements
    then [elements]
    else do
      let elementsLength = length elements
      let delimIndex = fromMaybe elementsLength (elemIndex delim elements)
      let currentElements = take (if delimIndex == elementsLength then 0 else delimIndex) elements
      let restOfElements = drop (delimIndex + 1) elements
      currentElements : splitOn delim restOfElements
