module test
import StdEnv
import StdString

alphabet :: [{#Char}]
alphabet = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

alphaNum :: [Int]
alphaNum = [91..122]

dictionary :: [{#Char}] [Int] -> [({#Char},Int)] 
dictionary alpha num = zip2 alpha num

alphaToInt :: {#Char} [({#Char},Int)] -> Int
alphaToInt charIn dict = hd[value\\(key,value)<-dict|key==charIn]

Start = [alphaToInt newChar (dictionary alphabet alphaNum)\\newChar<-fromString "hello"]