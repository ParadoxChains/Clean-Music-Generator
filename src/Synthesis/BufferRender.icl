implementation module Synthesis.BufferRender
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Util.ArrayUtils
import Util.ListUtils
import Synthesis.Envelope
import Synthesis.Generate
import Input.ReadFile
import Synthesis.PhaseAmplitudeConverter

generateSilence :: Int -> [Real]
generateSilence silenceSamples = [0.0 \\ x <- [1,2..silenceSamples]]

renderIndex :: Int NoteChunk -> Real
renderIndex globalTime chunk 
| localTime < 0  = 0.0
= envelope * wave * (toReal chunk.note.veolocity)
where
	localTime = (globalTime - (noteToSamples (convertDurToBeats chunk.note.initialTime chunk.timeSig) chunk.timeSig chunk.tempo))
    chunkBeats = (convertDurToBeats chunk.note.duration chunk.timeSig)
	sampleNum = noteToSamples chunkBeats chunk.timeSig chunk.tempo
    wave = generateLocal localTime chunk.wave chunk.note.frequency 
    envelope = getLocalDAHDSR localTime chunkBeats chunk.timeSig chunk.tempo chunk.dahdsr 

renderBuffer :: Int Int [NoteChunk] -> [Real]
renderBuffer left right chunkList = renderedTrack
where
    renderedTrack = [sum [renderIndex x chunk \\ chunk <- chunkList] \\ x <-[left,(left+1)..right]];

inInterval :: Int Int NoteChunk -> Bool
inInterval left right chunk = (noteLeft <= right) && (noteRight >= left)
where
    noteLeft = noteToSamples (convertDurToBeats chunk.note.initialTime chunk.timeSig) chunk.timeSig chunk.tempo
    noteRight = numberOfSamples chunk (chunk.note.initialTime+chunk.note.duration)

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
    x = hd chunkList
	bs = (noteToSamples (convertDurToBeats (x.timeSig.barVal * 4 * 24) x.timeSig) x.timeSig x.tempo)
    totalSamples = maxList [numberOfSamples x (x.note.initialTime+x.note.duration) \\ x <- chunkList]
    renderedTrack = flatten [renderBuffer (i*bs) (max ((i+1)*bs) totalSamples) (filter (inInterval (i*bs) (max ((i+1)*bs) totalSamples)) chunkList) \\ i <- [0,1..(totalSamples/bs)]]
    normalized = normalizeList renderedTrack (maxList [abs x \\ x <- renderedTrack])


render :: [Note] ChannelProfile -> [Real]
render noteList chanProf = renderAux chunkList
where
	chunkList = [noteToChunk nt chanProf \\ nt <- noteList]

noteToChunk :: Note ChannelProfile -> NoteChunk
noteToChunk nt chanProf = {note = nt, wave = chanProf.wavType, timeSig = nt.ts, tempo = nt.temp, dahdsr = chanProf.envelope}