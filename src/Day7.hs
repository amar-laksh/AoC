{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7 (day7) where

import Common (count, splitOn)
import Data.List (intercalate, sort)

readTerminal = readFile

prefix indents = replicate indents ' '

dirFormat d = "- " <> d <> " (dir)\n"

data Stack t = File t Int | Node t [Stack t] deriving (Eq, Ord, Show)

printStack = showTree 0
  where
    showTree indents (File n i) = prefix indents <> "- " <> n <> " (file, size=" <> show i <> ")"
    showTree indents (Node n fs) = prefix indents <> dirFormat n <> "\n" <> unlines [showTree (indents + 1) f | f <- fs]

printOutput stack = sequence_ [putStrLn ln | ln <- filter (/= "") (lines (printStack stack))]

-- parseLine :: String -> [Stack String]
-- parseLine line = do
--   let lns = drop 1 (lines line)
--   sort [parseLn (words ln) | ln <- lns]
--   where
--     parseLn ln
--       | head ln == "dir" = last ln
--       | otherwise = File (last ln) (read (head ln) :: Int)

buildTree :: [String] -> [Stack String] -> Stack String
buildTree [] _ = Node "" []
buildTree output parentTree = Node "" []

day7 = do
  print "***Day 7***"
  terminalOutput <- readTerminal "./inputs/input7.demo"
  let inputOutputs = filter (/= []) (splitOn '$' terminalOutput)
  let tlst = map (\x -> if head x == ' ' then drop 1 x else x) inputOutputs
  let t = Node "/" [Node "lol" [File "sgas.lol" 12545], File "t.txt" 12, File "tas.txt" 125, Node "emptyDir" [Node "testDir" [File "123.txt" 115152]]]
  printOutput t
