import Test.QuickCheck

multiRecursive :: Int -> Int -> Int
multiRecursive _ 0 = 0
multiRecursive 0 _ = 0
multiRecursive m n
  | m > 0 && n > 0 = m + multiRecursive m (n-1)
  | otherwise = error "Arguments must be positive integers"

prop_divisors m n = (m > 0 && n > 0) ==> (result `mod` m == 0) && (result `mod` m == 0)
  where result = multiRecursive m n
