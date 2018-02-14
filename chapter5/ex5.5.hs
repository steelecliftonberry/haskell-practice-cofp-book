data Shape = Circle Float |
             Rectangle Float Float
             deriving (Eq, Ord, Show)

perimeter :: Shape -> Float
perimeter (Circle r) = 2*pi*r
perimeter (Rectangle h w) = h*2+w*2
