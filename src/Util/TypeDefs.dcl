definition module Util.TypeDefs
import StdEnv


//Frequency of note
:: Frequency :== Real

//Number of samples
:: Samples :== Int

//Wavetype to be generated for note.
:: Wave = Sine | Square | Triangle | Noise | Pulse | Sawtooth | Silence

//MIDI Instruction
:: Channel :== Int

//MIDI Instruction
:: Velocity :== Int

//MIDI Instruction
/*NB: Duration is NEITHER number of samples nor seconds.
Each duration increment is 1/24th of a Beat, and
must be converted appropriately.*/
:: Duration :== Int

// A byte is a character
:: Byte :== Char

// Specifies whether a number is unsigned or signed
:: Signedness = Unsigned | Signed

// Specifies whether a number is big-endian or little-endian
:: Endianness = BE | LE

:: BitVersion = Eight | Sixteen | ThirtyTwo

// Sum of delta time
:: AccumulatedTime :== Int

// Strating time of an event
:: InitialTime :== Int

:: Tempo :== Real

:: Time = {minutes :: Real
          ,seconds :: Real
          }

:: TimeSignature = {barVal :: Int
                   ,noteVal :: Int}

:: Beat = {p :: Int,
           q :: Int}

// Strating time of a time signature event and time signature
:: TSEvent :== (InitialTime, TimeSignature)

// A list of time signature events
:: TSEvents :== [TSEvent]

// Starting time of a tempo event and the tempo value
:: TPEvent :== (InitialTime, Int)

// A list of tempo events
:: TPEvents :== [TPEvent]

// Flanger Parameters
:: FlangerParameters :== (Real, Real, Real)

// Chorus Parameters
:: ChorusParameters :== (Real, Real, Real, Real)

// Delay Parameters
:: DelayParameters :== (Real, Real)

// Effect
:: Effect = (Flanger, FlangerParameters) | (Chorus, ChorusParameters) | (Delay, DelayParameters) | (Phaser, PhaserParameters)
