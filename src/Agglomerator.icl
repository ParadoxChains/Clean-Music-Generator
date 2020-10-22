implementation module Agglomerator

import StdEnv, StdFile
import Input.MIDI.Chunks, Input.MIDI.ReadFile
import Output.MiddleLayer, Output.Pcm
import Synthesis.Accesstable, Synthesis.CasioEnvelope, Synthesis.Envelope,  Synthesis.GeneralEnvelope, Synthesis.Generate, Synthesis.Wave, Synthesis.Wavetable
//import Synthesis.BufferRender
import Synthesis.InlineRender
import Util.Byte, Util.Constants, Util.ListUtils, Util.Monad, Util.Rand, Util.TimeUtils, Util.TypeDefs

/*
//debug, file read
LetsGo :: String String ADSR Wave BitVersion !*World -> [Note]
LetsGo inFile outFile env1 wavType bits w
    #! (_, f, w) = fopen outFile FWriteData w
    #! (w, noteData) = read w inFile
    #! newChannelProfile = constructChannelProfile env1 wavType
    = noteData
*/

/*
//debug, render.
LetsGo :: String String ADSR Wave BitVersion !*World -> [Real]
LetsGo inFile outFile env1 wavType bits w
    #! (_, f, w) = fopen outFile FWriteData w
    #! (w, noteData) = read w inFile
    #! newChannelProfile = constructChannelProfile env1 wavType
    = render noteData newChannelProfile
*/


LetsGo :: String String ADSR Wave BitVersion !*World -> *World
LetsGo inFile outFile env1 wavType bits w
    #! (_, f, w) = fopen outFile FWriteData w
    #! (w, noteData) = read w inFile
    #! newChannelProfile = constructChannelProfile env1 wavType
    #! realsData = render noteData newChannelProfile
    #! data = transformOneChannel realsData 1.0 bits
    #! f = writePcmWav
        { numChannels    = 1
        , numBlocks      = (length realsData / 1)
        , samplingRate   = SAMPLING_RATE
        , bytesPerSample = (translatingBitVersion bits)/BYTE_SIZE
        } data f
    #! (_, w) = fclose f w
    = w

Diagnostics :: [String] String ADSR Wave BitVersion !*World -> *World
Diagnostics inFiles outFile env1 wavType bits w
    #! (_, f, w) = fopen outFile FWriteData w
    // #! (w, noteData) = read w inFile
    // #! noteCount = length noteData
    // #! newChannelProfile = constructChannelProfile env1 wavType
    // #! samplesCount = renderTotalSamples noteData newChannelProfile
    // #! data = makeDiagsString inFile noteCount samplesCount
    #! (w, data) = makeDiags inFiles env1 wavType bits w ""
    #! f = writeDiag data f
    #! (_, w) = fclose f w
    = w

makeDiags :: [String] ADSR Wave BitVersion !*World String -> (*World, String)
makeDiags [] env1 wavType bits w acc = (w,acc)
makeDiags [first:rest] env1 wavType bits w acc
    #! (w, noteData) = read w first
    #! noteCount = length noteData
    #! newChannelProfile = constructChannelProfile env1 wavType
    #! samplesCount = renderTotalSamples noteData newChannelProfile
    #! data = makeDiagsString first noteCount samplesCount
    = makeDiags rest env1 wavType bits w (acc +++ data +++ "\n")

makeDiagsString :: String Int Int -> String
makeDiagsString inFile notes samples = "==File Diagnostics==\n" +++ "    File name: " +++ inFile +++ "\n" +++ "    Notes read from file: " +++ (toString notes) +++ "\n" +++ "    Samples rendered: " +++ (toString samples) +++ "\n"

writeDiag :: !String !*File -> *File
writeDiag diags f
  #! f = fwrites diags f
  = f


constructChannelProfile :: ADSR Wave -> ChannelProfile
constructChannelProfile env1 wT = out
where
    newEnv = ADSRtoDAHDSR env1
    out = {wavType = wT, envelope = newEnv}

read :: !*World String -> (*World, [Note])
read w fileName
    #! (openStatus, ourFile, w) = fopen fileName FReadData w
    |not openStatus = (w, abort"File can not be opened\n")
    #! (byteList, ourFile) = readBytes ourFile
    #! (closeStatus, w) = fclose ourFile w
    | not closeStatus = (w, abort "File closing operation failed\n")
    = (w, readFile byteList)
