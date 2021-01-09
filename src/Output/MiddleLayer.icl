implementation module Output.MiddleLayer
import StdEnv
import Util.ListUtils, Util.Byte, Util.TypeDefs, Util.Constants

transformOneChannel :: [Real] !Real !BitVersion -> [Byte]
transformOneChannel list max bitVer
= flatten (map (\x = toBytes Signed LE (translatedBitVersion/BYTE_SIZE) x) (map (\x = movingWave x max bitVer) list))
    where
        translatedBitVersion = translatingBitVersion bitVer

transformTwoChannels :: [[Real]] !Real !BitVersion -> [[Byte]]
transformTwoChannels list max bitVer = map (\x = transformOneChannel x max bitVer) list

movingWave :: !Real !Real !BitVersion -> Int
movingWave targeted_number max bitVer
| translated_bit_version == 8 = toInt(255.0*((targeted_number/(1.0*max)+0.5)))
= movingWaveAux targeted_number max translated_bit_version
    where
        translated_bit_version = translatingBitVersion bitVer

movingWaveAux :: !Real !Real !Int -> Int
movingWaveAux targeted_number max bitVer
| targeted_number == max = 2^(bitVer-1)-1
= floor((targeted_number*2.0^toReal(bitVer-1))/max)

translatingBitVersion :: !BitVersion -> Int
translatingBitVersion Eight = 8
translatingBitVersion Sixteen = 16
translatingBitVersion ThirtyTwo = 32

