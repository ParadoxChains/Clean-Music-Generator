definition module Data.Byte

:: Byte :== Char

// Converts a non-negative integer to a given amount of bytes in little-endian
// The first parameter is the number of bytes
// The second parameter is the integer to be converted
natToBytesLE :: !Int !Int -> [Byte]
