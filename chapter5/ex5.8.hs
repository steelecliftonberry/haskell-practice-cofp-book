data Shape = Circle Float |
             Rectangle Float Float |
             Triangle Float Float Float
             deriving (Eq, Ord, Show)

isRegular :: Shape -> Bool
isRegular (Circle _) = True
isRegular (Rectangle h w)
  | h == w = True
  | otherwise = False
isRegular (Triangle l m n)
  | (l == m) && (m == n) = True
  | otherwise = False
