implementation module Output.MiddleLayer
import StdEnv

transform ::[Real] Real->[Char]
transform list max =map (\x=toChar(toInt(255.0*((x/(1.0*max)+0.5))))) list 

transform2 :: [[Real]] Real -> [[Char]]
transform2 list max = map (\x = transform x max) list 