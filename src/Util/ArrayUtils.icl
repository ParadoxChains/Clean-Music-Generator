implementation module Util.ArrayUtils
import StdEnv

smolArr :: *{Int}
smolArr = {x\\x<-[1..10]}
arr100::*{Int}
arr100 = {1\\x<-:smolArr,y<-:smolArr}
arr10000::*{Int}
arr10000 = {1\\x<-:arr100,y<-:arr100}
arr100000000 :: *{Int}
arr100000000 = {1\\x<-:arr10000,y<-:arr10000}
testArr1 :: *{Int}
testArr1 = {1,2,3,4,5}
testArr2 :: *{Int}
testArr2 = {1,2,3}

/*Function to generate an array sequence
Arguments: 
    Tuple containing: (Start, End, Increment)*/
arrSeq :: !(!Int,!Int,!Int) -> *(a Int) | Array a Int
arrSeq (start,end,inc)
| (start < end) <> (inc > 0) = arrSeq (start,end,(-1 * inc))
#!len = ((end-start+1)/inc)
= arrSeqAux (start,end,inc) len 0 (createArray len start)
/*where
    len = ((end-start+1)/inc)*/

arrSeqAux :: !(!Int,!Int,!Int) !Int !Int !*(a Int) -> *(a Int) | Array a Int
//arrSeqAux (start,end,inc) len index arr = {arr & [i]=elem \\ elem <- [start,(start+inc)..end] & i <- [0..(len-1)]}

arrSeqAux (start,end,inc) len index arr
    | index >= len = arr
    = arrSeqAux (start,end,inc) len (index+1) (update arr index (start + (inc*index)))

/*Function to reverse an entire array.
Arguments: Array*/
reverseArr :: !*(a e) -> *(a e) | Array a e
reverseArr a
    # (l, a) = usize a
    = reverseArrAux a 0 (l - 1)

/*Function to reverse a portion of an array.
Arguments: Array StartIndex EndIndex*/
reverseArrAux :: !*(a e) !Int !Int -> *(a e) | Array a e
reverseArrAux a i n
    #! st = a.[i]
    #!(en,a) = replace a n st
    #!(x,a) = replace a i en
    | i < n = reverseArrAux a (i + 1) (n - 1)
    = a

/*Function to circle rotate an array.
Arguments: Array nRotations sizeOfArray*/
shiftArr :: !*(a e) !Int -> *(a e) | Array a e
shiftArr a i
    #!(l,a) = usize a
    = reverseArrAux (reverseArrAux (reverseArrAux a 0 (i-1)) i (l-1)) 0 (l-1)

/*Function to add two arrays in parallel.
Arguments: Array1 Array2 Position
Position indicates which position of Array 1
we should start adding values from Array 2*/
/*stackArr :: *(a e) *(a e) Int -> *(a e) | Array a e
stackArr arr1 arr2 index
| index > 0
    # len1 = size arr1
    //# (len1, arr1) = usize arr1
    # len2 = size arr2
    //# (len2, arr2) = usize arr2
    # newLen = max len1 (len2 + index - 1)
    # newArr = createArray newLen arr1.[0]
    # newArr = {update newArr z arr1.[z]\\z<-[0..(index-1)]}
    = newArr
= stackArr arr2 arr1 (-1 * index)*/

//Array utilities
addArrAux :: !Int !{Real} !{Real} -> Real
addArrAux x a b 
	| (x >= (size a)) = b.[x-(size a)]
	= a.[x]

// ++
addArr :: !{Real} !{Real} -> {Real}
addArr a b = { addArrAux x a b \\ x <- [0,1..((size a) + (size b) - 1)]}

// last
lastArr :: !{Real} -> Real
lastArr a = a.[(size a) - 1]

// take
takeArr :: !Int !{Real} -> {Real} 
takeArr x a
	| x >= (size a) = a
	= {a.[ind] \\ ind <- [0,1..((size a) - 1)]}

//list to array
listToArr :: ![a] -> {a}
listToArr l = {x \\ x<-l}