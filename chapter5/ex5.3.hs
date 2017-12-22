import Test.QuickCheck

lineXIntercept :: Double -> Double -> (Double, Double)
lineXIntercept m b = ((-b)/m, 0)

prop_line_equation m b = m /= 0 ==> (abs $ m*(fst (lineXIntercept m b)) + b) <= tolerance
  where tolerance = 0.00001
