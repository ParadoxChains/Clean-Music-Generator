module scrap
import StdEnv

// (<) infix 4 :: [a] [a] -> Bool | <, == a
// (<) [] [] = False
// (<) [] _ = True
// (<) _ [] = False
// (<) [x : xs] [y : ys] = x < y //|| ( x == y && xs < ys)

// Start = [1, 2, 3, 4] < [1, 2, 3, 5]

// Start = foldr (\x y = x + y) 4 [1, ~2, 3]

SaveFile :: [Real] String *Int *World-> *World
SaveFile img fname mode w
# (ok, file, w) = fopen fname mode w
| not ok = abort "Can't open file"
# file = WriteFile file img
# (fail, file) = ferror file
| fail = abort "Couldn't write"
# (ok, w) = fclose file w
= w
		
WriteFile :: *File [Real]-> *File
WriteFile file img
// Example of writing to file.
#file = foldl (\x y = x <<< y) file img
= file

// Sample Write Start		
// Start :: *World -> *World
// // Start w = SaveFile [1.0, 3.0, 4.2, 42.0] "result.ppm" FWriteData w
// Start w = SaveFile [1, 3, 42, 420] "result.ppm" FWriteData w

ReadFile :: *File -> ([Real], *File)
ReadFile file
# (isEnd, file) = fend file
| isEnd = ([], file)
// ** From a datafile freadr will just read eight bytes (a Clean Real). **
// ** From a datafile freadi will just read four bytes (a Clean Int). **
# (ok, cur, file) = freadr file
| not ok = abort "Reading error, please check the input"
// # (ok, cur1, file) = freadi file
// | not ok = abort "Reading error, please check the input"
# (res, file) = ReadFile file
= ([cur] ++ res, file)
// = ([cur], file)
// = ([cur, cur1], file)

LoadObj :: String *World -> ([Real], *World)
LoadObj fname w
# (ok, file, w) = fopen fname mode w
| not ok = abort "Cant open"
# (content, file) = ReadFile file
//# (content, file) = (CreateTriangle content, file)
# (ok, w) = fclose file w // Always remember to close
| not ok = abort "Cant close"
= (content, w)
where
	mode = FReadData

// // Sample Read Start		
Start :: *World -> *World
Start w
// # w = SaveFile [1, 3, 42, 420] "result.ppm" FWriteData w
# w = SaveFile [1.0, 3.14, 4.2, 42.0, 2.5] "result.ppm" FWriteData w
# (content, w) = LoadObj "result.ppm" w
= SaveFile content "real.txt" FWriteText w
