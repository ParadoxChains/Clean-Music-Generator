implementation module Synthesis.ArrayRender
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Util.ArrayUtils
import Util.ListUtils
import Synthesis.EnvelopeArr
import Synthesis.Generate
import Input.ReadFile

// Arr
generateSilence :: Int -> {Real}
generateSilence silenceSamples = {0.0 \\ x <- [1,2..silenceSamples]}

// Arr
renderNoteChunk :: NoteChunk -> {Real}
renderNoteChunk chunk = applyEnvelope wave envByValue
where
    chunkBeats = (convertDurToBeats chunk.note.duration chunk.timeSig)
	sampleNum = noteToSamples chunkBeats chunk.timeSig chunk.tempo
	wave = generate chunk.wave chunk.note.frequency sampleNum
	envelope = getDAHDSR chunkBeats chunk.timeSig chunk.tempo chunk.dahdsr 
	envByValue = {x*(toReal chunk.note.veolocity) \\ x <-: envelope}

// NONE
numberOfSamples :: NoteChunk Int -> Int
numberOfSamples x dur = (noteToSamples (convertDurToBeats dur x.timeSig) x.timeSig x.tempo) + releaseSamples
where
	releaseSamples = (floor (1.0 / (toReal (secondsToSamples x.dahdsr.release)))) + 1

// ARR
normalizeList :: {Real} -> {Real}
normalizeList track = {x/peak \\ x <-: track}
where
	peak = maxList [abs x \\ x <-: track] // **

// ARR
renderAux :: [NoteChunk] -> {Real} // **
renderAux chunkList = normalized
where
	renderedTrack = {((numberOfSamples x x.note.initialTime), (renderNoteChunk x)) \\ x <- chunkList}
	totalSamples = maxList [numberOfSamples x (x.note.initialTime+x.note.duration) \\ x <- chunkList] // **
	noteSum = sumUp 0 (generateSilence totalSamples) renderedTrack
	normalized = normalizeList noteSum

// ARR
sumUp:: Int {Real} {(Int,{Real})} -> {Real}
sumUp i track arr
    | size arr == 0 = track
    | i == (size arr) = track
    = sumUp (i+1) newTrack arr
where
    sumAux jj (offset,tr)
        | jj <= offset = 0.0
        = tr.[jj-offset-1]
    newTrack = {track.[j]+(sumAux j arr.[i]) \\ j <- [0,1..((size track) - 1)]}

// ARR
render :: [Note] ChannelProfile -> {Real} // **
render noteList chanProf = renderAux chunkList
where
	chunkList = [noteToChunk nt chanProf \\ nt <- noteList]

// NONE
noteToChunk :: Note ChannelProfile -> NoteChunk
noteToChunk nt chanProf = {note = nt, wave = chanProf.wavType, timeSig = nt.ts, tempo = nt.temp, dahdsr = chanProf.envelope}
where
	ts = {barVal = 1, noteVal = 1}
	env = {delay = 0.0, attack = 1.0, hold = 0.0, decay = 2.0, sustain = 0.3, release = 1.0}
	