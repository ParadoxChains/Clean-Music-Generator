implementation module Output.MiddleLayer
import StdEnv
import Util.ListUtils, Util.Byte, Util.TypeDefs, Util.Constants

transform_one_channel :: [Real] Real BitVersion -> [Byte]
transform_one_channel list max bitVersion
= flatten (map (\x = toBytes Signed LE (translated_bit_version/BYTE_SIZE) x) (map (\x = moving_wave x max bitVersion) list))
    where
        translated_bit_version = translating_bit_version bitVersion

transform_two_channels :: [[Real]] Real BitVersion -> [[Byte]]
transform_two_channels list max bitVersion = map (\x = transform_one_channel x max bitVersion) list

moving_wave :: Real Real BitVersion -> Int
moving_wave targeted_number max bitVersion
| translated_bit_version == 8 = toInt(255.0*((targeted_number/(1.0*max)+0.5)))
= moving_wave_aux targeted_number max translated_bit_version
    where
        translated_bit_version = translating_bit_version bitVersion

moving_wave_aux :: Real Real Int -> Int
moving_wave_aux targeted_number max bitVersion
| targeted_number == max = 2^(bitVersion-1)-1
= floor((targeted_number*2.0^toReal(bitVersion-1))/max)

translating_bit_version :: BitVersion -> Int
translating_bit_version Eight = 8
translating_bit_version Sixteen = 16
translating_bit_version ThirtyTwo = 32

