module Day2
  ( readStratergy,
    outcome,
    handPlayed,
    newOutcome,
  )
where

data Player = Me | Opponent

data Move = Rock | Paper | Scissors | Invalid deriving (Eq, Ord)

data Result = Lost | Draw | Won | Undecidable deriving (Eq)

type Symbol = String

type Options = [Symbol]

type Stratergy = [(String, String)]

type Round = (String, String)

type Points = Int

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

toMove :: Symbol -> Player -> Move
toMove symbol player
  | handPlayed player symbol == Rock = Rock
  | handPlayed player symbol == Paper = Paper
  | handPlayed player symbol == Scissors = Scissors
  | otherwise = Invalid

toResult :: Symbol -> Result
toResult symbol
  | symbol == "X" = Lost
  | symbol == "Y" = Draw
  | symbol == "Z" = Won
  | otherwise = Undecidable

resultFromMoves :: Move -> Move -> Result
resultFromMoves myMove oppMove
  | myMove == Rock && oppMove == Paper = Lost
  | myMove == Rock && oppMove == Rock = Draw
  | myMove == Rock && oppMove == Scissors = Won
  | myMove == Paper && oppMove == Paper = Draw
  | myMove == Paper && oppMove == Rock = Won
  | myMove == Paper && oppMove == Scissors = Lost
  | myMove == Scissors && oppMove == Paper = Won
  | myMove == Scissors && oppMove == Rock = Lost
  | myMove == Scissors && oppMove == Scissors = Draw
  | otherwise = Undecidable

moveFromResult :: Result -> Move -> Move
moveFromResult result oppMove
  | result == Won && oppMove == Rock = Paper
  | result == Draw && oppMove == Rock = Rock
  | result == Lost && oppMove == Rock = Scissors
  | result == Won && oppMove == Paper = Scissors
  | result == Draw && oppMove == Paper = Paper
  | result == Lost && oppMove == Paper = Rock
  | result == Won && oppMove == Scissors = Rock
  | result == Draw && oppMove == Scissors = Scissors
  | result == Lost && oppMove == Scissors = Paper
  | otherwise = Invalid

resultPoints :: Result -> Points
resultPoints result
  | result == Won = 6
  | result == Draw = 3
  | result == Lost = 0
  | otherwise = 0

movePoints :: Move -> Points
movePoints move
  | move == Rock = 1
  | move == Paper = 2
  | move == Scissors = 3
  | otherwise = 0

outcome :: Round -> Points
outcome round
  | resultFromMoves myMove oppMove == Won = resultPoints Won + movePoints myMove
  | resultFromMoves myMove oppMove == Draw = resultPoints Draw + movePoints myMove
  | resultFromMoves myMove oppMove == Lost = resultPoints Lost + movePoints myMove
  | otherwise = 0
  where
    oppMove = toMove (fst round) Opponent
    myMove = toMove (snd round) Me

newOutcome :: Round -> Points
newOutcome round
  | moveFromResult result oppMove == Rock = movePoints Rock + resultPoints result
  | moveFromResult result oppMove == Paper = movePoints Paper + resultPoints result
  | moveFromResult result oppMove == Scissors = movePoints Scissors + resultPoints result
  | otherwise = 0
  where
    oppMove = toMove (fst round) Opponent
    result = toResult (snd round)

readStratergy :: String -> IO Stratergy
readStratergy filename = do
  map ((\x -> (head x, last x)) . words) . lines <$> readFile filename
