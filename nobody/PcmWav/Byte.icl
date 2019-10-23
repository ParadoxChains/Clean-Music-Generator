implementation module PcmWav.Byte

import StdEnv

:: Byte :== Char

neIntToBytesLE :: !Int !Int -> [Byte]
neIntToBytesLE i n = take i (go n) where
  go n = [toChar (n bitand 255) : go (n >> 8)]
