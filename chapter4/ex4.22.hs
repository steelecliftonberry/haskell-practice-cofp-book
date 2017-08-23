anyOutputsZero :: (Integer -> Integer) -> Integer -> Bool
anyOutputsZero f n = elem 0 $ map f $ enumFromTo 0 n

anyOutputsZero' :: (Integer -> Integer) -> Integer -> Bool
anyOutputsZero' f n
  | n < 0 = False
  | f n == 0 = True
  | otherwise = anyOutputsZero' f (n-1)
