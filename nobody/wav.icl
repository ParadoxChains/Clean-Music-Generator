module wav

import StdFile

// The first parameter is the size of the file minus 8 bytes.
writeHeader :: !Int !*File -> *File
writeHeader l f = f <<< "RIFF" <<< l <<< "WAVE"

test :: !*World -> *World
test w
  #! (_, f, w) = fopen "test.wav" FWriteData w
  #! f = writeHeader 256 f
  #! (_, w) = fclose f w
  = w

Start w = test w
