module Day2
  ( readStratergy,
    outcome,
    handPlayed,
  )
where

data Player = Me | Opponent

data Move = Rock | Paper | Scissors | Invalid deriving (Eq)

type Symbol = String

type Options = [Symbol]

optionsFor :: Player -> Options
optionsFor Me = do
  ["X", "Y", "Z"]
optionsFor Opponent = do
  ["A", "B", "C"]

rockSymbol :: Player -> Symbol
rockSymbol Me = head (optionsFor Me)
rockSymbol Opponent = head (optionsFor Opponent)

paperSymbol :: Player -> Symbol
paperSymbol Me = optionsFor Me !! 1
paperSymbol Opponent = optionsFor Opponent !! 1

scissorsSymbol :: Player -> Symbol
scissorsSymbol Me = last (optionsFor Me)
scissorsSymbol Opponent = last (optionsFor Opponent)

handPlayed :: Player -> Symbol -> Move
handPlayed Me symbol
  | rockSymbol Me == symbol = Rock
  | paperSymbol Me == symbol = Paper
  | scissorsSymbol Me == symbol = Scissors
  | otherwise = Invalid
handPlayed Opponent symbol
  | rockSymbol Opponent == symbol = Rock
  | paperSymbol Opponent == symbol = Paper
  | scissorsSymbol Opponent == symbol = Scissors
  | otherwise = Invalid

myHand :: Symbol -> Move
myHand symbol
  | handPlayed Me symbol == Rock = Rock
  | handPlayed Me symbol == Paper = Paper
  | handPlayed Me symbol == Scissors = Scissors
  | otherwise = Invalid

opponentHand :: Symbol -> Move
opponentHand symbol
  | handPlayed Opponent symbol == Rock = Rock
  | handPlayed Opponent symbol == Paper = Paper
  | handPlayed Opponent symbol == Scissors = Scissors
  | otherwise = Invalid

rockPoints = 1

paperPoints = 2

scissorsPoints = 3

lost = 0

draw = 3

won = 6

type Stratergy = [(String, String)]

type Round = (String, String)

type Points = Int

outcome :: Round -> Points
outcome round
  | opponentHand oppPlay == Rock && myHand myPlay == Rock = draw + rockPoints
  | opponentHand oppPlay == Rock && myHand myPlay == Paper = won + paperPoints
  | opponentHand oppPlay == Rock && myHand myPlay == Scissors = lost + scissorsPoints
  | opponentHand oppPlay == Paper && myHand myPlay == Rock = lost + rockPoints
  | opponentHand oppPlay == Paper && myHand myPlay == Paper = draw + paperPoints
  | opponentHand oppPlay == Paper && myHand myPlay == Scissors = won + scissorsPoints
  | opponentHand oppPlay == Scissors && myHand myPlay == Rock = won + rockPoints
  | opponentHand oppPlay == Scissors && myHand myPlay == Paper = lost + paperPoints
  | opponentHand oppPlay == Scissors && myHand myPlay == Scissors = draw + scissorsPoints
  | otherwise = 0
  where
    myPlay = snd round
    oppPlay = fst round

readStratergy :: String -> IO Stratergy
readStratergy filename = do
  map ((\x -> (head x, last x)) . words) . lines <$> readFile filename
