implementation module readFile

import StdEnv
import StdFile

getNoteNumber :: [Char] -> Char
getNoteNumber [] = abort "lack of info"
getNoteNumber [s,n:cs] 
	#!status = toInt s / 16
	|status == 8 || status == 9 = n
	= getNoteNumber [n:cs]
Start = toInt (getNoteNumber [toChar 144, toChar 9])

//assuming equal tuning based on A4=a'=440 Hz
noteToFrequency :: Int -> Real
noteToFrequency n 
	|n >= 0 || n <= 127 = 440.0 * 2.0 ^ (toReal(n-69) / 12.0)
	= abort "incorrect MIDI note number"
	
//byte order:big endian
byteToInt :: [Char] -> Int
byteToInt [] = 0
byteToInt [c:cs]
	#!len = length cs
	= toInt c * 2 ^ (8*len) + byteToInt cs

readBytes :: *File -> ([Char], *File)
readBytes oldF 
	#! (b, c, newF) = freadc oldF
	|not b = ([], newF)
	#! (l, f) = readBytes newF
	= ([c:l], f)

read :: !*World -> (*World, [Char])
read oldW
	#! (b, oldF, newW) = fopen "bytes.txt" FReadData oldW
	|not b = (newW, [])
	#! (l, newF) = readBytes oldF
	#! (b, newW2) = fclose newF newW
	= (newW2, l)
	
//Start w = read w