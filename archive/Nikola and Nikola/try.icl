module try
import StdEnv
times127::[[Real]]->[[Real]]
times127 []=[]
times l = map (\a = map (\y = (y*127.0)) a) l
//Start=fun [[1.2,3.4,4.5],[1.2,3.4,4.5]]
//Start=fun[[0.6,1.0,0.6,-0.3,-0.5,-1.0,-0.5,0.0,0.3,0.6,1.0,0.6,-0.3,-0.5,-1.0,-0.5,0.0,0.3,0.6,1.0,0.6,-0.3,-0.5,-1.0,-0.5,0.0,0.3,0.6,1.0,0.6,-0.3,-0.5,-1.0,-0.5,0.0,0.3,0.6,1.0,0.6,-0.3,-0.5,-1.0,-0.5,0.0,0.3,0.6,1.0,0.6,-0.3,-0.5,-1.0,-0.5,0.0,0.3],[1.0,0.8,0.5,0.3,0.0,-0.3,-0.5,-0.7,-0.4,0.0,0.3,0.5,0.7,1.0]]
//converting from a number to a list of 1's and 0's
decimalToBinary::Int->[Int]
decimalToBinary 0=[0]
decimalToBinary x=decimalToBinary (x/2) ++[abs(x rem 2)]
//adding zeroes or ones if the length of the list is less than 8
decimalToBinary8bits::Int->[Int]
decimalToBinary8bits x
|length p<8 && x<0=[1\\y<-[1..(8- (length p))]]++ p
|length p<8 && x>=0=[0\\y<-[1..(8- (length p))]]++ p
= p
where
	p=decimalToBinary x
//Start=decimalToBinary8bits -8 
//Start=reverse(addOne(reverse(convert(decimalToBinary8bits -34 ))))

//changing 1 to 0 and 0 to 1
convert::[Int]->[Int]
convert []=[]
convert [x:xs]
|x==1=[0]++convert xs
=[1]++convert xs
//Adding one 
addOne ::[Int]->[Int]
addOne []=[1]
addOne[x:xs]
|x==0=[1]++xs
=[0]++ addOne xs
SecondComplement::Int->Int
SecondComplement x
|x>=0=sum[y*10^n\\y<-(decimalToBinary x)& n<-[0..]]
=sum[y*10^ n\\ y<-((addOne(reverse(convert(decimalToBinary8bits x )))))& n<-[0..]]
//Start=decimalToBinary -34

//Start=reverse (addOne (reverse(decimalToBinary 35)))
Start=SecondComplement -127
//Start=SecondComplement 4
