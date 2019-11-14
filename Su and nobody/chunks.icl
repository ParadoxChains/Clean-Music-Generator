module chunks

import StdEnv
isHeader :: [Char] -> Bool
isHeader l
	|length l < 4 = False 
	#! type = toString (take 4 l)
	|type == "MThd" = True
	= False

isTrack :: [Char] -> Bool
isTrack l
	|length l < 4 = False 
	#! type = toString (take 4 l)
	|type == "MTrk" = True
	= False

byteToInt :: [Char] -> Int
byteToInt [] = 0
byteToInt [c:cs]
	#! len = length cs
	= toInt c * 2 ^ (7*len) + byteToInt cs

deltaTimeList :: [Char] -> [Char]
deltaTimeList [] = []
deltaTimeList [c:cs]
	#! d = toInt c
	|d < 128 = [c]
	= [c:deltaTimeList cs]

deltaTime :: [Char] -> Int
deltaTime = byteToInt o deltaTimeList

Start = deltaTime(map toChar[129,128,1])