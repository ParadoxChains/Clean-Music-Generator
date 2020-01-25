definition module Synthesis.CasioEnvelope

// rate :: % per second, level :: %
:: CasioCZ = {rate1 :: Real ,level1 :: Real // Front Envelope
             ,rate2 :: Real ,level2 :: Real 
   	         ,rate3 :: Real ,level3 :: Real
   	         ,rate4 :: Real ,level4 :: Real
   	         ,rate5 :: Real ,level5 :: Real // ~Front Envelope
   	         ,rate6 :: Real ,level6 :: Real // Release Envelope
   	         ,rate7 :: Real ,level7 :: Real
   	         ,rate8 :: Real ,level8 :: Real // ~Release Envelope
             }    

getCasioCZ :: Real CasioCZ -> [Real]


