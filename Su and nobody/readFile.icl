module readFile

import StdEnv
import StdFile

//byte order:big endian
byteToInt :: [Char] -> Int
byteToInt [c:cs] = toInt c 

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
	
Start w = read w