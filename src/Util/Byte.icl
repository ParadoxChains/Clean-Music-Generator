implementation module Util.Byte

import StdEnv
import StdFile

:: Byte :== Char

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
