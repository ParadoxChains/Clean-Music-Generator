implementation module utils
import StdEnv

// Rotate a list N places to the left. 
phaseShift :: [Real] Int -> [Real]
phaseShift [] _ = []
phaseShift x 0 = x
phaseShift x y
| y > 0 = phaseShift (tl x ++ [hd x]) (y-1)
= phaseShift [last x: init x] (y+1)

// floor function
floor :: Real -> Int
floor r
| toReal (toInt r) > r = (toInt r) - 1
= toInt r

// rem for Real numbers
myRem :: Real Real -> Real
myRem a b = b * abs(c - toReal(floor c))
where 
    c = a / b

// sums up two lists
sumLists :: [Real] [Real] -> [Real]
sumLists a b = [ i + j \\ i <- a & j <- b]
// subtracts two lists
subtractLists :: [Real] [Real] -> [Real]
subtractLists a b = [ i - j \\ i <- a & j <- b]

// sum all lists
sumAll :: [[Real]] -> [Real]
sumAll [] = []
sumAll [x] = x
sumAll [x:xs] = sumLists x (sumAll xs)