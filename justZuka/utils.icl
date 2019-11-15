implementation module utils
import StdEnv
// Rotate a list N places to the left. 
phaseShift :: [Real] Int -> [Real]
phaseShift [] _ = []
phaseShift x 0 = x
phaseShift x y
| y > 0 = phaseShift (tl x ++ [hd x]) (y-1)
= phaseShift [last x: init x] (y+1)


floor :: Real -> Int
floor r
| toReal (toInt r) > r = (toInt r) - 1
= toInt r

myRem :: Real Real -> Real
myRem a b = b * abs(c - toReal(floor c))
where 
    c = a / b