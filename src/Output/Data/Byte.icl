implementation module Data.Byte

import StdEnv

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
