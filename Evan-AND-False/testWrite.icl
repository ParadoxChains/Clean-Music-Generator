module testWrite

import StdEnv
import sinewave


WriteFile :: *File String -> *File
WriteFile file outString
//# file = file <<< "TEST HEADER\n"
// Write a [Int] to a file
# file = foldl (\x y = x <<< y <<< ' ') file outData
= file
	where 
		//outData = [alphaToInt newChar (dictionary alphabet alphaNum)\\newChar<-fromString outString] //converting String to Int then writing out
        //outData = [1..100] //just writing out numbers 1 to 100
		//outData = [32..122] //ASCII Keyboard numbers
		//outData = [16391,1073741824]//numbers suggested by Nghia 2^14+7 and 2^30
		outData = reverseMap

newSine :: [Int]
newSine = map toInt (map ((+)(((122.0 - 97.0)/2.0) + 97.0)) (generateSine ((122.0 - 97.0)/2.0)))

reverseMap :: [{#Char}]
reverseMap = [alphabet!!(x-97)\\x<-newSine]

ourString :: String
ourString = "a"

alphabet :: [{#Char}]
alphabet = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

alphaNum :: [Int]
alphaNum = [97..122]

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
