import Test.QuickCheck

-- Using Euclid's algorithm for GCD:
myGcd :: Integral a => a -> a -> a
myGcd 0 b = b
myGcd a 0 = a
myGcd a b = myGcd b r
  where r = a `mod` b

test1 :: Bool
test1 = myGcd 270 192 == 6

prop_answer_divides_both_inputs a b = a > 0 && b > 0 ==> (a `mod` (myGcd a b) == 0) && (b `mod` (myGcd a b) == 0)

prop_gcd_positive a b = a > 0 && b > 0 ==> myGcd a b > 0

prop_myGcd_equals_stdlib_gcd a b = a > 0 && b > 0 ==> myGcd a b == gcd a b
