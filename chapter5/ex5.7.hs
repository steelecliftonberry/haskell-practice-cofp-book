data Shape = Circle Float |
             Rectangle Float Float |
             Triangle Float Float Float
             deriving (Eq, Ord, Show)

perimeter :: Shape -> Float
perimeter (Circle r) = 2*pi*r
perimeter (Rectangle h w) = h*2+w*2
perimeter (Triangle l m n) = l+m+n

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) =  False
isRound (Triangle _ _ _) = False

-- Use Heron's formula for area of triangle given 3 sides
area :: Shape -> Float
area (Circle r) = pi*r^2
area (Rectangle h w) = h*w
area (Triangle l m n) = sqrt(s*(s-l)*(s-m)*(s-n))
  where s = (l+m+n)/2
