implementation module Synthesis.Render
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Util.ArrayUtils
import Synthesis.Envelope
import Synthesis.Generate
import Input.MIDI.ReadFile


generateSilence :: Int -> [Real]
generateSilence silence_samples = [0.0 \\ x <- [1,2..silence_samples]]


renderNoteChunk :: NoteChunk -> [Real]
renderNoteChunk chunk = applyEnvelope wave env_by_value
where
    chunk_beats = (convertDurToBeats chunk.note.duration chunk.timeSig)
	sample_num = noteToSamples chunk_beats chunk.timeSig chunk.tempo
	wave = generate chunk.wave chunk.note.frequency _n
	envelope = getDAHDSR chunk_beats chunk.timeSig chunk.tempo chunk.dahdsr 
	env_by_value = [x*(toReal chunk.note.veolocity) \\ x <- envelope]


numberOfSamples :: NoteChunk Int -> Int
numberOfSamples x dur = (noteToSamples (convertDurToBeats dur x.timeSig) x.timeSig x.tempo) + release_samples
where
	release_samples = floor (1.0 / (toReal secondsToSamples x.dahdsr.release)) + 1


normalizeList :: [Real] -> [Real]
normalizeList track = [x/peak \\ x <- track]
where
	peak = maxList [abs x \\ x <- track]

renderAux :: [NoteChunk] -> [Real]
renderAux chunk_list = normalized
where
	total_samples = maxList [numberOfSamples x (x.note.initialTime+x.note.duration) \\ x <- chunk_list]
	silence_track = generateSilence total_samples
	rendered_track = [(generateSilence (numberOfSamples x x.note.initialTime)) ++ (renderNoteChunk x) ++
	                 (generateSilence (total_samples - (numberOfSamples x (x.note.initialTime+x.note.duration)))) \\ x <- chunk_list]
	rendered_track_arr = [listToArr ls \\ ls <- rendered_track]
	note_sum = [(sum [arr.[ind] \\ arr <- rendered_track_arr]) \\ ind <- [0,1..(total_samples-1)]]
	normalized = normalizeList note_sum

render :: [Note] ChannelProfile -> [Real]
render note_list chan_prof = renderAux chunk_list
where
	chunk_list = [noteToChunk nt chan_prof \\ nt <- note_list]

noteToChunk :: Note ChannelProfile -> NoteChunk
noteToChunk nt chan_prof = {note = nt, wave = chan_prof.wavType, timeSig = nt.ts, tempo = nt.temp, dahdsr = chan_prof.envelope}
where
	ts = {barVal = 1, noteVal = 1}
	env = {delay = 0.0, attack = 1.0, hold = 0.0, decay = 2.0, sustain = 0.3, release = 1.0}
