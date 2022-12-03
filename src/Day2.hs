module Day2
  ( readStratergy,
    winningSchemeFor,
    toSchemeBasedList,
    applyStratergy1,
    Player (..),
  )
where

import Common (indexOf)

data Player = Me | Opponent

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

toSchemeBasedList :: StratergyList -> [(Int, Int)]
toSchemeBasedList stratergyList = do
  let myScheme = winningSchemeFor Me
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
  | myMove - opMove == -2 = movePoints myMove + resultPoints Lost
  | myMove > opMove = movePoints myMove + resultPoints Lost
  | myMove < opMove = movePoints myMove + resultPoints Won
  | otherwise = 0

totalPoints :: Int -> Int -> Int
totalPoints opMove myMove
  | myMove == opMove = movePoints myMove + resultPoints Draw
  | myMove - opMove == 2 = movePoints myMove + resultPoints Won
  | myMove - opMove == -2 = movePoints myMove + resultPoints Lost
  | myMove > opMove = movePoints myMove + resultPoints Lost
  | myMove < opMove = movePoints myMove + resultPoints Won
  | otherwise = 0

readStratergy :: String -> IO StratergyList
readStratergy filename = do
  map ((\x -> (head x, last x)) . words) . lines <$> readFile filename
