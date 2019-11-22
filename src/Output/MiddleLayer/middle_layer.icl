implementation module middle_layer
import StdEnv

transform ::[Real] Real->[Char]
transform list max =map (\x=toChar(toInt(255*((x/(2.0*max)+0.5)))) list 
//transform2 ::[[Real]] Real->[[Real]]
//transform2 list max=map (\x=transform x max) list 