implementation module utils

import StdEnv

//byte order in MIDI:big endian (MSB first)
byteToInt :: [Char] -> Int
byteToInt [] = 0
byteToInt [c:cs]
	#! len = length cs
	= toInt c * 2 ^ (8*len) + byteToInt cs
