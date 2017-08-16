import Test.QuickCheck

data Result = Win | Lose | Draw
  deriving (Show, Eq)

data Move = Rock | Paper | Scissors
  deriving (Show, Eq)

-- For QuickCheck to work over the Move type.
instance Arbitrary Move where
  arbitrary = elements [Rock, Paper, Scissors]

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper

-- QuickCheck test
prop_beat_lose_link move = lose (beat move) == move

outcome :: Move -> Move -> Main.Result
outcome move1 move2
  | move1 == move2 = Draw
  | (lose move1) == move2 = Win
  | otherwise = Lose

outcome' :: Move -> Move -> Main.Result
outcome' Rock Scissors = Win
outcome' Rock Paper = Lose
outcome' Rock Rock = Draw
outcome' Scissors Rock = Lose
outcome' Scissors Paper = Win
outcome' Scissors Scissors = Draw
outcome' Paper Rock = Win
outcome' Paper Scissors = Lose
outcome' Paper Paper = Draw

prop_outcome move1 move2 = (outcome move1 move2) == (outcome' move1 move2)
