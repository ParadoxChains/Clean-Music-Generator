implementation module Synthesis.Render
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Util.ArrayUtils
import Synthesis.Envelope
import Synthesis.Generate
import Input.ReadFile


generateSilence :: Int -> [Real]
generateSilence silenceSamples = [0.0 \\ x <- [1,2..silenceSamples]]


renderNoteChunk :: NoteChunk -> [Real]
renderNoteChunk chunk = applyEnvelope wave envByValue
where
    chunkBeats = (convertDurToBeats chunk.note.duration chunk.timeSig)
	sampleNum = noteToSamples chunkBeats chunk.timeSig chunk.tempo
	wave = generate chunk.wave chunk.note.frequency sampleNum
	envelope = getDAHDSR chunkBeats chunk.timeSig chunk.tempo chunk.dahdsr 
	envByValue = [x*(toReal chunk.note.veolocity) \\ x <- envelope]


numberOfSamples :: NoteChunk Int -> Int
numberOfSamples x dur = (noteToSamples (convertDurToBeats dur x.timeSig) x.timeSig x.tempo) + releaseSamples
where
	releaseSamples = floor (1.0 / (toReal secondsToSamples x.dahdsr.release)) + 1


normalizeList :: [Real] -> [Real]
normalizeList track = [x/peak \\ x <- track]
where
	peak = maxList [abs x \\ x <- track]

renderAux :: [NoteChunk] -> [Real]
renderAux chunkList = normalized
where
	totalSamples = maxList [numberOfSamples x (x.note.initialTime+x.note.duration) \\ x <- chunkList]
	silenceTrack = generateSilence totalSamples
	renderedTrack = [(generateSilence (numberOfSamples x x.note.initialTime)) ++ (renderNoteChunk x) ++
	                 (generateSilence (totalSamples - (numberOfSamples x (x.note.initialTime+x.note.duration)))) \\ x <- chunkList]
	renderedTrackArr = [listToArr ls \\ ls <- renderedTrack]
	noteSum = [(sum [arr.[ind] \\ arr <- renderedTrackArr]) \\ ind <- [0,1..(totalSamples-1)]]
	normalized = normalizeList noteSum

// ---------Possibly more efficient implementation----------

sumUp :: (Int,[Real]) (Int,{Real}) -> [Real]
sumUp (totalSamples,mainTrack) (offset,track) = (totalSamples,newTrack)
where
	newTrack = [mS+((index < offset) ? 0 : track.[index]) \\ mS <- mainTrack & index <-[0,1..totalSamples]]
//					^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
// Sorry for C++ syntax, I could not remember lambda functions syntax

renderSecondAux :: [NoteChunk] -> [Real]
renderSecondAux chunkList = normalized
where
	totalSamples = maxList [numberOfSamples x (x.note.initialTime+x.note.duration) \\ x <- chunkList]
	silenceTrack = generateSilence totalSamples
	renderedTrack = [((numberOfSamples x x.note.initialTime),(renderNoteChunk x)) \\ x <- chunkList]
	renderedTrackArr = [(fst(ls),listToArr (snd ls)) \\ ls <- renderedTrack]
	noteSum = foldLeft sumUp (totalSamples,silenceTrack) renderedTrack // Also, I do not remember foldLeft(or foldRight?)
	normalized = normalizeList noteSum

// --------------------------------------------------------

render :: [Note] ChannelProfile -> [Real]
render noteList chanProf = renderAux chunkList
where
	chunkList = [noteToChunk nt chanProf \\ nt <- noteList]

noteToChunk :: Note ChannelProfile -> NoteChunk
noteToChunk nt chanProf = {note = nt, wave = chanProf.wavType, timeSig = nt.ts, tempo = nt.temp, dahdsr = chanProf.envelope}
where
	ts = {barVal = 1, noteVal = 1}
	env = {delay = 0.0, attack = 1.0, hold = 0.0, decay = 2.0, sustain = 0.3, release = 1.0}
