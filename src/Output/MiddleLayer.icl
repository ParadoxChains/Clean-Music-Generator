implementation module Output.MiddleLayer
import StdEnv
import Util.ListUtils, Util.Byte, Util.TypeDefs, Util.Constants

transformOneChannel :: [Real] Real BitVersion -> [Byte]
transformOneChannel list max bit_version
= flatten (map (\x = toBytes Signed LE (translated_bit_version/BYTE_SIZE) x) (map (\x = movingWave x max bitVersion) list))
    where
        translated_bit_version = translatingBitVersion bit_version

transformTwoChannels :: [[Real]] Real BitVersion -> [[Byte]]
transformTwoChannels list max bit_version = map (\x = transformOneChannel x max bit_version) list

movingWave :: Real Real BitVersion -> Int
movingWave targeted_number max bit_version
| translated_bit_version == 8 = toInt(255.0*((targeted_number/(1.0*max)+0.5)))
= movingWaveAux targeted_number max translated_bit_version
    where
        translated_bit_version = translatingBitVersion bit_version

movingWaveAux :: Real Real Int -> Int
movingWaveAux targeted_number max bit_version
| targeted_number == max = 2^(bit_version-1)-1
= floor((targeted_number*2.0^toReal(bit_version-1))/max)

translatingBitVersion :: BitVersion -> Int
translatingBitVersion Eight = 8
translatingBitVersion Sixteen = 16
translatingBitVersion ThirtyTwo = 32

