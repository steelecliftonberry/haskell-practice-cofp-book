import Test.QuickCheck

multiRecursive :: Int -> Int -> Int
multiRecursive m n = foldr (+) 0 $ take m $ repeat n

prop_divisors m n = (m > 0 && n > 0) ==> (result `mod` m == 0) && (result `mod` m == 0)
  where result = multiRecursive m n
