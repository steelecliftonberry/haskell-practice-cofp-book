import Test.QuickCheck

-- Using Euclid's algorithm for GCD:
twoToThePower :: Integral a => a -> a
twoToThePower 0 = 1
twoToThePower n
  | n `mod` 2 == 1 = 2*(twoToThePower ((n - 1) `div` 2))^2
  | otherwise = (twoToThePower (n `div` 2))^2

test1 :: Bool
test1 = twoToThePower 10 == 1024

test2 :: Bool
test2 = twoToThePower 9 == 512

prop_twoToThePower_equals_stdlib_exponentiation n = n >= 0 ==> twoToThePower n == 2^n
