implementation module Agglomerator

import StdEnv, StdFile
import Input.Chunks, Input.ReadFile
import Output.MiddleLayer, Output.Pcm
import Synthesis.Accesstable, Synthesis.CasioEnvelope, Synthesis.Envelope,  Synthesis.GeneralEnvelope, Synthesis.Generate, Synthesis.Render, Synthesis.Wave, Synthesis.Wavetable
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


//debug, render.
LetsGo :: String String ADSR Wave BitVersion !*World -> [Real]
LetsGo inFile outFile env1 wavType bits w
    #! (_, f, w) = fopen outFile FWriteData w
    #! (w, noteData) = read w inFile
    #! newChannelProfile = constructChannelProfile env1 wavType
    = render noteData newChannelProfile


/*
LetsGo :: String String ADSR Wave BitVersion !*World -> *World
LetsGo inFile outFile env1 wavType bits w
    #! (_, f, w) = fopen outFile FWriteData w
    #! (w, noteData) = read w inFile
    #! newChannelProfile = constructChannelProfile env1 wavType
    #! realsData = render noteData newChannelProfile
    #! data = transform_one_channel realsData 0.75 bits
    #! f = writePcmWav
        { numChannels    = 1
        , numBlocks      = (length data / 1)
        , samplingRate   = SAMPLING_RATE
        , bytesPerSample = (translating_bit_version bits)/BYTE_SIZE
        } data f
    #! (_, w) = fclose f w
    = w
*/
constructChannelProfile :: ADSR Wave -> ChannelProfile
constructChannelProfile env1 wT = out
where
    newEnv = ADSRtoDAHDSR env1
    out = {wavType = wT, envelope = newEnv}

read :: !*World String -> (*World, [Note])
read w fileName
    #! (openStatus, ourFile, w) = fopen fileName FReadData w
    |not openStatus = (w, abort"File can not be opened")
    #! (byteList, ourFile) = readBytes ourFile
    #! (closeStatus, w) = fclose ourFile w
    | not closeStatus = (w, abort "File closing operation failed")
    = (w, readFile byteList)
