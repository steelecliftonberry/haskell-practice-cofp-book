data Result = Win | Lose | Draw

data Move = Rock | Paper | Scissors

outcome :: Move -> Move -> Result
outcome Rock Scissors = Win
outcome Rock Paper = Lose
outcome Rock Rock = Draw
outcome Scissors Rock = Lose
outcome Scissors Paper = Win
outcome Scissors Scissors = Draw
outcome Paper Rock = Win
outcome Paper Scissors = Lose
outcome Paper Paper = Draw
