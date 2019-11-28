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

lengthArr :: (a e) -> Int | Array a e
lengthArr arr = sum[1\\x<-:arr]

/*
Start :: {Int}
Start = arr100000000
*/
//Start = lengthArr arr100000000

/*
reverseArr :: *(a e) -> *(a e) | Array a e
//reverseArr :: *{a} -> *{a}
reverseArr a = reverseArrAux a 0 ((lengthArr a)-1)
*/
reverseArrAux :: *(a e) Int Int -> *(a e) | Array a e
reverseArrAux a i n
    #! st = a.[i]
    #! en = a.[n]
    | i < n = reverseArrAux {a & [i] = en, [n] = st} (i+1) (n-1)
    = a
/*
Start :: {Int}
Start = reverseArrAux arr100000000 0 ((lengthArr arr100000000)-1)
*/

shiftArr :: *(a e) Int Int-> *(a e) | Array a e
shiftArr a i n = reverseArrAux (reverseArrAux (reverseArrAux a 0 (i-1)) i (n-1)) 0 (n-1)
/*
where
    n = (lengthArr a) - 1
*/

Start :: {Int}
//Start = reverseArrAux(reverseArrAux(reverseArrAux {1,2,3,4,5,6,7,8,9,10} 0 3) 4 9) 0 9
//Start = reverseArrAux(reverseArrAux(reverseArrAux arr100 0 3) 4 99) 0 99
//Start = shiftArr {1,2,3,4,5,6,7,8,9,10} 4 10
Start = shiftArr arr100000000 4 (lengthArr arr100000000)
/*
    b = reverseArrAux a 0 (i-1)
    c = reverseArrAux b i n
    d = reverseArrAux c 0 n
*/
//Start = 1
/*
void rvereseArray(int arr[], int start, int end) 
{ 
    while (start < end) { 
        int temp = arr[start]; 
        arr[start] = arr[end]; 
        arr[end] = temp; 
        start++; 
        end--; 
    } 
} 
  

void leftRotate(int arr[], int d, int n) 
{ 
    if (d == 0) 
        return; 
    rvereseArray(arr, 0, d - 1); 
    rvereseArray(arr, d, n - 1); 
    rvereseArray(arr, 0, n - 1); 
} 
*/