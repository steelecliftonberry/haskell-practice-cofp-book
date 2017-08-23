import Test.QuickCheck

naiveIntegerSquareRoot :: Integer -> Integer
naiveIntegerSquareRoot n = naiveIntegerSquareRootAcc n 1

naiveIntegerSquareRootAcc :: Integer -> Integer -> Integer
naiveIntegerSquareRootAcc n m
  | n <= 0 || m <= 0 = error "Arguments must be positive integers"
  | m*m <= n && (m+1)*(m+1) > n = m
  | otherwise = naiveIntegerSquareRootAcc n (m+1)

prop_nothing_higher n = n > 0 ==>
  result*result <= n && (result+1)*(result+1) > n
    where result = naiveIntegerSquareRoot n
