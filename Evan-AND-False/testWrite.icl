module testWrite

import StdEnv


WriteFile :: *File String -> *File
WriteFile file outString
//# file = file <<< "TEST HEADER\n"
// Write a [Int] to a file
# file = foldl (\x y = x <<< y <<< ' ') file outData
= file
	where 
		//outData = [alphaToInt newChar (dictionary alphabet alphaNum)\\newChar<-fromString outString] //converting String to Int then writing out
        //outData = [1..100] //just writing out numbers 1 to 100
		outData = [32..122]

ourString :: String
ourString = "a"

alphabet :: [{#Char}]
alphabet = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

alphaNum :: [Int]
alphaNum = [91..122]

dictionary :: [{#Char}] [Int] -> [({#Char},Int)] 
dictionary alpha num = zip2 alpha num

alphaToInt :: {#Char} [({#Char},Int)] -> Int
alphaToInt charIn dict = hd[value\\(key,value)<-dict|key==charIn]

SaveToFile :: String String *World -> *World
SaveToFile outString fname w
# (ok, file, w) = fopen fname mode w
| not ok = abort "Can't open file"
# file = WriteFile file outString
# (fail, file) = ferror file
| fail = abort "Couldn't write"
# (ok, w) = fclose file w
= w
	where
		mode = FWriteData
		
Start w = SaveToFile ourString "test.txt" w
