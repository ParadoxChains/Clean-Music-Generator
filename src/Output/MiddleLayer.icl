implementation module Output.MiddleLayer
import StdEnv
import Util.ListUtils
import Util.Byte

transform8 ::[Real] Real->[Char]
transform8 list max =map (\x=toChar(toInt(255.0*((x/(1.0*max)+0.5))))) list 

transform8_what_if :: [[Real]] Real -> [[Char]]
transform8_what_if list max = map (\x = transform8 x max) list 


aux16 :: Real Real -> Int
aux16 x max
|x==max=2^15-1
=floor((x*2.0^15.0)/max)

transform16 :: [Real] Real -> [Char]
transform16 list max=flatten (map (\x = toBytes Signed LE 2 x) (map (\x = aux16 x max) list))

transform16_what_if::[[Real]] Real->[[Char]]
transform16_what_if listLists max =map (\x = transform16 x max) listLists


aux32::Real Real->Int
aux32 x max
|x==max=2^31-1
=floor((x*2.0^31.0)/max)

transform32::[Real] Real->[Char]
transform32 list max=flatten (map (\x = toBytes Signed LE 4 x) (map (\x = aux32 x max) list))

transform32_what_if::[[Real]] Real->[[Char]]
transform32_what_if listLists max =map (\x = transform32 x max) listLists
