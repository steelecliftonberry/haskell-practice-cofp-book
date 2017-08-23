import Test.QuickCheck
import Data.List

maxOfFunctionOutputs :: (Integer -> Integer) -> Integer -> Integer
maxOfFunctionOutputs f n = last $ sort $ map f (enumFromTo 1 n)
