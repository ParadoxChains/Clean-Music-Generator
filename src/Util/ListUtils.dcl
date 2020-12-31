definition module Util.ListUtils

// Rotate a list N places to the left.
shiftLeft :: [a] !Int -> [a]

// floor function
floor :: !Real -> Int

// rem for Real numbers
realRem :: !Real !Real -> Real

// sums up two lists
sumLists :: [Real] [Real] -> [Real]

// subtracts two lists
subtractLists :: [Real] [Real] -> [Real]

// sum all lists
sumAll :: [[Real]] -> [Real]


//array to list
strToList :: !String -> [Char]
