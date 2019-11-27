definition module Util.Byte

// A byte is a character
:: Byte :== Char

// Take the complement of a byte
complement :: !Byte -> Byte

// Converts a non-negative integer to a given amount of bytes in big-endian
// The first parameter is the number of bytes
// The second parameter is the integer to be converted
uintToBytesBE :: !Int !Int -> [Byte]

// Converts a non-negative integer to a given amount of bytes in little-endian
// The first parameter is the number of bytes
// The second parameter is the integer to be converted
uintToBytesLE :: !Int !Int -> [Byte]

// Converts a list of bytes to a non-negative integer in big-endian
bytesToUintBE :: ![Byte] -> Int

// Converts a list of bytes to a non-negative integer in little-endian
bytesToUintLE :: ![Byte] -> Int

// Converts a signed integer to a given amount of bytes in big-endian
// The first parameter is the number of bytes
// The second parameter is the integer to be converted
intToBytesBE :: !Int !Int -> [Byte]

// Converts a signed integer to a given amount of bytes in little-endian
// The first parameter is the number of bytes
// The second parameter is the integer to be converted
intToBytesLE :: !Int !Int -> [Byte]

// Converts a list of bytes to a signed integer in big-endian
bytesToIntBE :: ![Byte] -> Int

// Converts a list of bytes to a signed integer in little-endian
bytesToIntLE :: ![Byte] -> Int

// Reads every byte of a file
readBytes :: !*File -> ([Byte], !*File)

// Writes a list of bytes into a file
writeBytes :: ![Byte] !*File -> *File
