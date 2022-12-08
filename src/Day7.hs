module Day7 (day7) where

import Common (splitOn)
import GHC.Read (list)

readTerminal filename = do
  readFile filename

cmds = ["ls"]

paramCmds = ["cd"]

params = ["/", ".."]

listingTypes = ["dir"]

data Directory = Directory String | Root | BackDirectory | InvalidDirectory deriving (Show, Eq, Ord)

data Commands = ChangeDirectory Directory | ListDirectories | InvalidCommand deriving (Show, Eq, Ord)

data Output = DirectoryType Directory | File String | FileWithSize (String, Int) | DirectoryWithSize (String, Int) | InvalidOutput deriving (Show, Eq, Ord)

data Tree = PNode Output | CNode Output | Tree

getDirectory :: String -> Directory
getDirectory dir
  | dir == head params = Root
  | dir == last params = BackDirectory
  | otherwise = Directory dir

getListings :: [String] -> [Output]
getListings lst = do
  [parseListing l | l <- lst]
  where
    parseListing l
      | head (words l) == head listingTypes = DirectoryType (getDirectory (last (words l)))
      | '.' `notElem` last (words l) = DirectoryWithSize (last (words l), read (head (words l)) :: Int)
      | otherwise = FileWithSize (last (words l), read (head (words l)) :: Int)

parseOutput :: [String] -> (Commands, [Output])
parseOutput [paramCmd]
  | head (words paramCmd) == head paramCmds = (ChangeDirectory (getDirectory (last (words paramCmd))), [])
parseOutput output
  | head output == head cmds = (ListDirectories, getListings (drop 1 output))
  | otherwise = (InvalidCommand, [InvalidOutput])

parseOutputs :: [[String]] -> [(Commands, [Output])]
parseOutputs outputs = do
  [parseOutput output | output <- outputs]

tree :: [(Commands, [Output])] -> [Commands]
tree lst = do
  [c | (c, o) <- lst, null o]

day7 = do
  print "***Day 7***"
  terminalOutput <- readTerminal "./inputs/input7.demo"
  let inputOutputs = filter (/= []) (splitOn '$' terminalOutput)
  let tlst = map (filter (/= "") . splitOn '\n' . (\x -> if head x == ' ' then drop 1 x else x)) inputOutputs
  let ast = parseOutputs tlst
  print (map tree [ast])

-- print (map tree (parseOutputs tlst))
