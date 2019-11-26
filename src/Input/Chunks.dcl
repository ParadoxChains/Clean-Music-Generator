definition module Input.Chunks

isHeader :: [Char] -> Bool

isTrack :: [Char] -> Bool

trackChunkLen :: [Char] -> Int

calcFormat :: [Char] -> Int

calcDivision :: [Char] -> Int

deltaTime :: [Char] -> (Int,Int)

firstHalfStatus :: Char -> Int

secondHalfStatus :: Char -> Int

isNoteOn :: Char -> Bool

isNoteOff :: Char -> Bool

getChannel :: Char -> Int

getFrequency :: Char -> Real

getVelocity :: Char -> Int
