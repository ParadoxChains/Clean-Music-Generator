implementation module Util.Byte

import StdEnv
import StdFile

:: Byte :== Char

complement :: !Byte -> Byte
complement b = toChar 255 - b

uintToBytesBE :: !Int !Int -> [Byte]
uintToBytesBE i n = go i n [] where
  go i n bs
    | i <= 0 = bs
             = go (i - 1) (n >> 8) [toChar (n bitand 255) : bs]

uintToBytesLE :: !Int !Int -> [Byte]
uintToBytesLE i n
  | i <= 0 = []
           = [toChar (n bitand 255) : uintToBytesLE (i - 1) (n >> 8)]

bytesToUintBE :: ![Byte] -> Int
bytesToUintBE bs = foldl (\n b. toInt b bitor (n << 8)) 0 bs

bytesToUintLE :: ![Byte] -> Int
bytesToUintLE bs = foldr (\b n. toInt b bitor (n << 8)) 0 bs

intToBytesBE :: !Int !Int -> [Byte]
intToBytesBE i n
  | n >= 0 = uintToBytesBE i n
           = map complement (uintToBytesBE i (~(n + 1)))

intToBytesLE :: !Int !Int -> [Byte]
intToBytesLE i n
  | n >= 0 = uintToBytesLE i n
           = map complement (uintToBytesLE i (~(n + 1)))

bytesToIntBE :: ![Byte] -> Int
bytesToIntBE bs
  #! n = bytesToUintBE bs
  #! p = 1 << ((length bs << 3) - 1)
  | n bitor p == 0 = n
                   = ~(bitnot n bitand (p - 1)) - 1

bytesToIntLE :: ![Byte] -> Int
bytesToIntLE bs
  #! n = bytesToUintLE bs
  #! p = 1 << ((length bs << 3) - 1)
  | n bitor p == 0 = n
                   = ~(bitnot n bitand (p - 1)) - 1

readBytes :: !*File -> ([Byte], !*File)
readBytes f
  #! (b, c, f) = freadc f
  | not b = ([], f)
  #! (cs, f) = readBytes f
  = ([c:cs], f)

writeBytes :: ![Byte] !*File -> *File
writeBytes []     f = f
writeBytes [b:bs] f 
  #! f = fwritec b f
  = writeBytes bs f
