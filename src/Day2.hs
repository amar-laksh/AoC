{-# OPTIONS_GHC -Wall #-}

-- TODO
--
--
--
--
--
--
--
--
--
--
--
--
--
-- THIS IS VERY BAD, GOTTA COMPLETELY REWRITE IT
module Day2
  ( readStratergy,
    winningSchemeFor,
    toSchemeBasedList,
    toSchemeBasedList2,
    applyStratergy1,
    applyStratergy2,
    Player (..),
    day2,
  )
where

import Common (indexOf)

data Player = Me | Opponent | Round deriving (Eq)

data Result = Lost | Draw | Won | Undecidable deriving (Eq)

type StratergyList = [(String, String)]

type Points = Int

resultPoints :: Result -> Points
resultPoints result
  | result == Won = 6
  | result == Draw = 3
  | result == Lost = 0
  | otherwise = 0

movePoints :: Int -> Points
movePoints move
  | move == 0 = 1
  | move == 1 = 3
  | move == 2 = 2
  | otherwise = 0

winningSchemeFor :: Player -> [String]
winningSchemeFor Opponent = do
  ["A", "C", "B"]
winningSchemeFor Me = do
  ["X", "Z", "Y"]
winningSchemeFor Round = do
  ["X", "", "", "Y", "", "", "Z"]

toSchemeBasedList :: StratergyList -> [(Int, Int)]
toSchemeBasedList stratergyList = do
  let myScheme = winningSchemeFor Me
  let opScheme = winningSchemeFor Opponent
  [(indexOf os opScheme, indexOf ms myScheme) | (os, ms) <- stratergyList]

toSchemeBasedList2 :: StratergyList -> [(Int, Int)]
toSchemeBasedList2 stratergyList = do
  let myScheme = winningSchemeFor Round
  let opScheme = winningSchemeFor Opponent
  [(indexOf os opScheme, indexOf ms myScheme) | (os, ms) <- stratergyList]

applyStratergy1 :: [(Int, Int)] -> [Points]
applyStratergy1 stratergyList = do
  [totalPoints opMove myMove | (opMove, myMove) <- stratergyList]

applyStratergy2 :: [(Int, Int)] -> [Points]
applyStratergy2 stratergyList = do
  [totalPoints2 opMove myMove | (opMove, myMove) <- stratergyList]

totalPoints2 :: Int -> Int -> Int
totalPoints2 opMove myMove
  | myMove == opMove = movePoints myMove + resultPoints Draw
  | myMove - opMove == 2 = movePoints myMove + resultPoints Won
  | myMove - opMove == -2 = movePoints myMove + resultPoints Draw
  | myMove > opMove = movePoints myMove + resultPoints Lost
  | myMove < opMove = movePoints myMove + resultPoints Won
  | otherwise = 0

totalPoints :: Int -> Int -> Int
totalPoints opMove myMove
  | myMove == opMove = movePoints myMove + resultPoints Draw
  | myMove == 2 && opMove == 0 = movePoints myMove + resultPoints Won
  | myMove == 0 && opMove == 2 = movePoints myMove + resultPoints Lost
  | myMove > opMove = movePoints myMove + resultPoints Lost
  | myMove < opMove = movePoints myMove + resultPoints Won
  | otherwise = 0

readStratergy :: String -> IO StratergyList
readStratergy filename = do
  map ((\x -> (head x, last x)) . words) . lines <$> readFile filename

day2 :: IO ()
day2 = do
  print "\n**Day 2***"
  rounds <- readStratergy "./inputs/input2.txt"
  print (sum (applyStratergy1 (toSchemeBasedList rounds)) == 11063)
  rounds2 <- readStratergy "./inputs/input2.demo"
  print (sum (applyStratergy2 (toSchemeBasedList2 rounds2)))
