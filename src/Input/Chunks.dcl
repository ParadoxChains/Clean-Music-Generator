definition module Input.Chunks
import Util.TypeDefs

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

isMeta :: Char -> Bool

isTempo :: [Char] -> Bool

isTimeSignature :: [Char] -> Bool

getChannel :: Char -> Int

getFrequency :: Char -> Frequency

getVelocity :: Char -> Int
