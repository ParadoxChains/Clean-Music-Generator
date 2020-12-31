implementation module Util.ListUtils
import StdEnv

// Rotate a list N places to the left.
shiftLeft :: [a] !Int -> [a]
shiftLeft [] _ = []
shiftLeft x 0 = x
shiftLeft x y = (drop shift x) ++ (take shift x)
where
    shift
        | y > 0 = (y rem (length x))
        = ((length x)-((~y) rem (length x)))

// Old floor function

floor :: !Real -> Int
floor r
| toReal (toInt r) > r = (toInt r) - 1
= toInt r

// rem for Real numbers
realRem :: !Real !Real -> Real
realRem a b = b * abs(c - toReal(floor c))
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

strToList :: !String -> [Char]
strToList arr = [x\\x<-:arr]
