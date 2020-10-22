implementation module Util.Buffering
import StdEnv


//to divide a list into current amount of blocks

buffer::[Real] Int -> [[Real]]
buffer list size
|size == 0 = []
|length list > size = [take size list] ++ (buffer (drop size list) size)
 =[list  ++ [0.0 \\a<-[1..(size - length list)]]]
