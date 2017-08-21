import Test.QuickCheck

rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
  | n < m = 0
  | otherwise = foldr (*) 1 $ enumFromTo m n

prop_rangeProduct_divisible m n = m > 0 && n > 0 ==> (result `mod` m == 0) && (result `mod` n == 0)
  where result = rangeProduct m n

prop_rangeProduct_positive m n = m > 0 && n > 0 ==> rangeProduct m n >= 0

fac :: Integer -> Integer
fac 0 = 1
fac n = rangeProduct 1 n

prop_fac_divisible n = n > 0 ==> (fac n) `mod` n == 0

prop_fac_positive n = n > 0 ==> fac n > 0
