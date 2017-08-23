import Data.List

maxOfFunctionOutputs :: (Integer -> Integer) -> Integer -> Integer
maxOfFunctionOutputs f n = last $ sort $ map f $ enumFromTo 0 n
