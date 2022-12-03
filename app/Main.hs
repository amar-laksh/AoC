module Main where

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3

main :: IO ()
main = do
  D1.day1
  D2.day2
  D3.day3

-- print (sum (map D2.applyResponseStratergy rounds) == 11063)
-- print (sum (map D2.applyConditionStratergy rounds) == 10349)
