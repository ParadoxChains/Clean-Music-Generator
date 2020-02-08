definition module Output.MiddleLayer
import Util.TypeDefs
import StdEnv


transform_one_channel :: [Real] Real BitVersion -> [Byte]

transform_two_channels :: [[Real]] Real BitVersion -> [[Byte]]

translating_bit_version :: BitVersion -> Int