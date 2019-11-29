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

/*Function to reverse an entire array.
Arguments: Array*/
reverseArr :: *(a e) -> *(a e) | Array a e
reverseArr a
    # (l, a) = usize a
    = reverseArrAux a 0 (l - 1)

/*Function to reverse a portion of an array.
Arguments: Array StartIndex EndIndex*/
reverseArrAux :: *(a e) Int Int -> *(a e) | Array a e
reverseArrAux a i n
    #! st = a.[i]
    #!(en,a) = replace a n st
    #!(x,a) = replace a i en
    | i < n = reverseArrAux a (i + 1) (n - 1)
    = a

/*Function to circle rotate an array.
Arguments: Array nRotations sizeOfArray*/
shiftArr :: *(a e) Int -> *(a e) | Array a e
shiftArr a i
    #!(l,a) = usize a
    = reverseArrAux (reverseArrAux (reverseArrAux a 0 (i-1)) i (l-1)) 0 (l-1)
