definition module Data.Byte

// A byte is a character
:: Byte :== Char

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
