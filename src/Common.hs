module Common (splitOn, indexOf, count, get3rd, takeInN, subset, intersect, replace, isSet, zipAll) where

import Data.List (elemIndex, transpose)
import Data.Maybe (fromMaybe)

get3rd (_, _, x) = x

zipAllWith :: ([a] -> b) -> [[a]] -> [b]
zipAllWith _ [] = []
zipAllWith f xss = map f . transpose $ xss

zipAll = zipAllWith id

isSet :: (Eq a) => [a] -> Bool
isSet [] = True
isSet (x : xs) = x `notElem` xs && isSet xs

inHead :: String -> String -> Bool
inHead needle haystack = take (length needle) haystack == needle

replace :: String -> String -> String -> String
replace needle replacement [] = []
replace needle replacement haystack
  | inHead needle haystack = replacement ++ replace needle replacement (drop (length needle) haystack)
  | not (inHead needle haystack) = head haystack : replace needle replacement (tail haystack)

indexOf :: Eq a => a -> [a] -> Int
indexOf delim elements = fromMaybe (length elements) (elemIndex delim elements)

subset :: Ord a => [a] -> [a] -> Bool
subset firstL secondL =
  all (`elem` secondL) firstL

intersect :: Ord a => [a] -> [a] -> Bool
intersect firstL secondL =
  any (`elem` secondL) firstL

count p xs = sum [1 | x <- xs, p x]

-- splitOn :: Char -> String -> [String]
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
