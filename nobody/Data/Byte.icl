implementation module Data.Byte

import StdEnv

:: Byte :== Char


/*
Transfiring an Int to a list of bytes
First parameter is the number of bytes we want to take and the second is the number we want to
tranfer 
*/
natToBytesLE :: !Int !Int -> [Byte]
natToBytesLE i n = take i (go n) where
  go n = [toChar (n bitand 255) : go (n >> 8)]
