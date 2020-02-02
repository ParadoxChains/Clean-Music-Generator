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
