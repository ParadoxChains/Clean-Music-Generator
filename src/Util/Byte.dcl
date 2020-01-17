definition module Util.Byte

// A byte is a character
:: Byte :== Char

// Specifies whether a number is unsigned or signed
:: Signedness = Unsigned | Signed

// Specifies whether a number is big-endian or little-endian
:: Endianness = BE | LE

// Take the complement of a byte
complement :: !Byte -> Byte

// Converts an integer to a given amount of bytes
// 1st parameter specifies whether the integer is signed or unsigned
// 2nd parameter sepcifies whether the integer is big or little-endian
// 3rd parameter specified the amount of bytes the output list should have
// 4th parameter is the integer to be converted
toBytes :: !Signedness !Endianness !Int !Int -> [Byte]

// Converts a list of bytes to an integer
// 1st parameter specifies whether the integer is signed or unsigned
// 2nd parameter sepcifies whether the integer is big or little-endian
// 3rd parameter is the list of bytes to be converted
fromBytes :: !Signedness !Endianness ![Byte] -> Int

// Reads every byte of a file
readBytes :: !*File -> ([Byte], !*File)

// Writes a list of bytes into a file
writeBytes :: ![Byte] !*File -> *File
