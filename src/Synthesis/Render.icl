implementation module Synthesis.Render
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Synthesis.Generate
import Input.ReadFile
import Util.ArrayUtils

generateSilence :: Int -> [Real]
generateSilence silenceSamples = [0.0 \\ x <- [1,2..silenceSamples]]


renderNoteChunk :: NoteChunk -> [Real]
renderNoteChunk chunk = applyEnvelope wave envelope
where
    chunkBeats = (convertDurToBeats chunk.note.duration chunk.timeSig)
	sampleNum = noteToSamples chunkBeats chunk.timeSig chunk.tempo
	wave = generate chunk.wave chunk.note.frequency sampleNum
	dahdsr = {delay = 0.0006, attack = 0.0003, hold = 0.0002, decay = 0.0006, sustain = 0.5, release = 0.0010}
	envelope = getDAHDSR chunkBeats chunk.timeSig chunk.tempo chunk.dahdsr 


numberOfSamples :: NoteChunk Int -> Int
numberOfSamples x dur = noteToSamples (convertDurToBeats dur x.timeSig) x.timeSig x.tempo


normalizeList :: [Real] -> [Real]
normalizeList track = [x/peak \\ x <- track]
where
	peak = maxList [abs x \\ x <- track]


render :: [NoteChunk] -> [Real]
render chunkList = normalized
where
	totalSamples = maxList [numberOfSamples x (x.note.initialTime+x.note.duration) \\ x <- chunkList]
	silenceTrack = generateSilence totalSamples
	renderedTrack = [(generateSilence (numberOfSamples x x.note.initialTime)) ++ (renderNoteChunk x) ++
	                 (generateSilence (totalSamples - (numberOfSamples x (x.note.initialTime+x.note.duration)))) \\ x <- chunkList]
	renderedTrackArr = [listToArr ls \\ ls <- renderedTrack]
	noteSum = [(sum [arr.[ind] \\ arr <- renderedTrackArr]) \\ ind <- [0,1..(totalSamples-1)]]
	normalized = normalizeList noteSum


