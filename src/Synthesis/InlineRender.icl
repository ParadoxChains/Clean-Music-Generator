implementation module Synthesis.InlineRender
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Util.ArrayUtils
import Util.ListUtils
import Synthesis.Envelope
import Synthesis.Generate
import Input.ReadFile

generateSilence :: Int -> [Real]
generateSilence silenceSamples = [0.0 \\ x <- [1,2..silenceSamples]]

renderIndex :: Int NoteChunk -> Real
renderIndex globalTime chunk 
| localTime < 0  = 0.0
= envelope * wave * (toReal chunk.note.veolocity)
where
	localTime = (globalTime - (noteToSamples (convertDurToBeats chunk.initialTime chunk.timeSig) chunk.timeSig chunk.tempo))
    chunkBeats = (convertDurToBeats chunk.note.duration chunk.timeSig)
	sampleNum = noteToSamples chunkBeats chunk.timeSig chunk.tempo
    wave = generateLocal localTime chunk.wave chunk.note.frequency sampleNum
    envelope = getLocalDAHDSR localTime chunkBeats chunk.timeSig chunk.tempo chunk.dahdsr 

numberOfSamples :: NoteChunk Int -> Int
numberOfSamples x dur = (noteToSamples (convertDurToBeats dur x.timeSig) x.timeSig x.tempo) + releaseSamples
where
	releaseSamples = (secondsToSamples x.dahdsr.release) + 1

normalizeList :: [Real] Real -> [Real]
normalizeList track peak = [x/safePeak \\ x <- track]
where
	safePeak | peak == 0.0 = 1.0 = peak;

renderAux :: [NoteChunk] -> [Real]
renderAux chunkList = normalized
where
	totalSamples = maxList [numberOfSamples x (x.note.initialTime+x.note.duration) \\ x <- chunkList]
    renderedTrack = [sum [renderIndex x chunk \\ chunk <- chunkList] \\ x <-[1,2..totalSamples]];
	normalized = normalizeList renderedTrack (maxList [abs x \\ x <- renderedTrack])


render :: [Note] ChannelProfile -> [Real]
render noteList chanProf = renderAux chunkList
where
	chunkList = [noteToChunk nt chanProf \\ nt <- noteList]

noteToChunk :: Note ChannelProfile -> NoteChunk
noteToChunk nt chanProf = {note = nt, wave = chanProf.wavType, timeSig = nt.ts, tempo = nt.temp, dahdsr = chanProf.envelope}
where
	ts = {barVal = 1, noteVal = 1}
	env = {delay = 0.0, attack = 1.0, hold = 0.0, decay = 2.0, sustain = 0.3, release = 1.0}