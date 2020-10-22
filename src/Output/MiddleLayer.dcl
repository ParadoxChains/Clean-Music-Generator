definition module Output.MiddleLayer
import Util.TypeDefs
import StdEnv


transformOneChannel :: [Real] Real BitVersion -> [Byte]

transformTwoChannels :: [[Real]] Real BitVersion -> [[Byte]]

translatingBitVersion :: BitVersion -> Int