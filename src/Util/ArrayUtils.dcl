definition module Util.ArrayUtils
import StdEnv

/*Function to generate an array sequence
Arguments: 
    Tuple containing: (Start, End, Increment)*/
arrSeq :: (Int,Int,Int) -> *(a Int) | Array a Int

/*Function to reverse an entire array.
Arguments: Array*/
reverseArr :: *(a e) -> *(a e) | Array a e

/*Function to reverse a portion of an array.
Arguments: Array StartIndex EndIndex*/
reverseArrAux :: *(a e) Int Int -> *(a e) | Array a e

/*Function to circle rotate an array.
Arguments: Array nRotations sizeOfArray*/
shiftArr :: *(a e) Int -> *(a e) | Array a e

//stackArr :: *(a e) *(a e) Int -> *(a e) | Array a e

addArrAux :: Int {Real} {Real} -> Real

// ++
addArr :: {Real} {Real} -> {Real}

// last
lastArr :: {Real} -> Real

// tale
takeArr :: Int {Real} -> {Real} 

//list to array
listToArr :: [a] -> {a}