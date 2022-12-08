module Day7 (day7) where

import Common (splitOn)
import GHC.Read (list)

readTerminal filename = do
  readFile filename

cmds = ["ls"]

paramCmds = ["cd"]

params = ["/", ".."]

data Commands = ChangeDirectory | ListDirectories | InvalidCommand deriving (Show, Eq, Ord)

data Directory = Directory String | Root | BackDirectory | InvalidDirectory deriving (Show, Eq, Ord)

data Output o = DirectoryType Directory | Files String | InvalidOutput deriving (Show, Eq, Ord)

getDirectory :: String -> Output String
getDirectory dir
  | dir == head params = DirectoryType Root
  | dir == last params = DirectoryType BackDirectory
  | otherwise = DirectoryType (Directory dir)

getListings :: String -> Output String
getListings lst = do
  Files lst

parseOutput :: [String] -> (Commands, Output String)
parseOutput [paramCmd]
  | head (words paramCmd) == head paramCmds = (ChangeDirectory, getDirectory (last (words paramCmd)))
parseOutput output
  | head output == head cmds = (ListDirectories, getListings (concatMap (++ " ") (drop 1 output)))
  | otherwise = (InvalidCommand, InvalidOutput)

parseOutputs :: [[String]] -> [(Commands, Output String)]
parseOutputs outputs = do
  [parseOutput output | output <- outputs]

day7 = do
  print "***Day 7***"
  terminalOutput <- readTerminal "./inputs/input7.demo"
  let inputOutputs = filter (/= []) (splitOn '$' terminalOutput)
  let tlst = map (filter (/= "") . splitOn '\n' . (\x -> if head x == ' ' then drop 1 x else x)) inputOutputs
  print (parseOutputs tlst)
