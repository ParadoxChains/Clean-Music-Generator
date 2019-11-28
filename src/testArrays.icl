module testArrays
import StdEnv, Util.ArrayUtils

smolArr :: *{Int}
smolArr = {x\\x<-[1..10]}
arr100::*{Int}
arr100 = {1\\x<-:smolArr,y<-:smolArr}
arr10000::*{Int}
arr10000 = {1\\x<-:arr100,y<-:arr100}
arr100000000 :: *{Int}
arr100000000 = {1\\x<-:arr10000,y<-:arr10000}

Start = 1