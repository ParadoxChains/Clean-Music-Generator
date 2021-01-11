implementation module Util.Byte

import StdEnv
import StdFile
import Util.Byte

complement :: !Byte -> Byte
complement b = toChar 255 - b

toBytesUnsignedBE :: !Int !Int -> [Byte]
toBytesUnsignedBE i n = go i n [] where
  go i n bs
    | i <= 0 = bs
             = go (i - 1) (n >> 8) [toChar (n bitand 255) : bs]

toBytesUnsignedLE :: !Int !Int -> [Byte]
toBytesUnsignedLE i n
  | i <= 0 = []
           = [toChar (n bitand 255) : toBytesUnsignedLE (i - 1) (n >> 8)]

toBytesSigned :: (Int Int -> [Byte]) !Int !Int -> [Byte]
toBytesSigned f i n
  | n >= 0 = f i n
           = map complement (f i (~(n + 1)))

fromBytesUnsignedBE :: ![Byte] -> Int
fromBytesUnsignedBE bs = foldl (\n b. toInt b bitor (n << 8)) 0 bs

fromBytesUnsignedLE :: ![Byte] -> Int
fromBytesUnsignedLE bs = foldr (\b n. toInt b bitor (n << 8)) 0 bs

fromBytesSigned :: ([Byte] -> Int) ![Byte] -> Int
fromBytesSigned f bs
  #! n = f bs
  #! p = 1 << ((length bs << 3) - 1)
  | n bitor p == 0 = n
                   = ~(bitnot n bitand (p - 1)) - 1

toBytes :: !Signedness !Endianness !Int !Int -> [Byte]
toBytes s e i n = go s e i n where
  go Unsigned BE = toBytesUnsignedBE
  go Unsigned LE = toBytesUnsignedLE
  go Signed   BE = toBytesSigned toBytesUnsignedBE
  go Signed   LE = toBytesSigned toBytesUnsignedLE

fromBytes :: !Signedness !Endianness ![Byte] -> Int
fromBytes s e bs = go s e bs where
  go Unsigned BE = fromBytesUnsignedBE
  go Unsigned LE = fromBytesUnsignedLE
  go Signed   BE = fromBytesSigned fromBytesUnsignedBE
  go Signed   LE = fromBytesSigned fromBytesUnsignedLE

readBytes :: !*File -> ([Byte], !*File)
readBytes f
  #! (cs, f) = go [] f
  = (reverse cs, f)
  where
  go cs f
    #! (b, c, f) = freadc f
    | not b = (cs, f)
    = go [c:cs] f

writeBytes :: [Byte] !*File -> *File
writeBytes []     f = f
writeBytes [b:bs] f
  #! f = fwritec b f
  = writeBytes bs f
