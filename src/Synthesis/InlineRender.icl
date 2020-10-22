implementation module Synthesis.InlineRender
import StdEnv
import Util.TypeDefs
import Util.TimeUtils
import Util.ArrayUtils
import Util.ListUtils
import Synthesis.Envelope
import Synthesis.Generate
import Input.MIDI.ReadFile
import Synthesis.PhaseAmplitudeConverter


generateSilence :: Int -> [Real]
generateSilence silence_samples = [0.0 \\ x <- [1,2..silence_samples]]

renderIndex :: Int NoteChunk -> Real
renderIndex global_time chunk 
| local_time < 0  = 0.0
= result_sample
where
	local_time = (global_time - (noteToSamples (convertDurToBeats chunk.note.initialTime chunk.timeSig) chunk.timeSig chunk.tempo))
    chunk_beats = (convertDurToBeats chunk.note.duration chunk.timeSig)
	sample_num = noteToSamples chunk_beats chunk.timeSig chunk.tempo
    wave = generateLocal local_time chunk.wave chunk.note.frequency 
    envelope = getLocalDAHDSR local_time chunk_beats chunk.timeSig chunk.tempo chunk.dahdsr 
	applied_envelope = envelope * wave * (toReal chunk.note.veolocity)
	result_sample = applied_envelope

numberOfSamples :: NoteChunk Int -> Int
numberOfSamples x dur = (noteToSamples (convertDurToBeats dur x.timeSig) x.timeSig x.tempo) + release_samples
where
	release_samples = (secondsToSamples x.dahdsr.release) + 1

normalizeList :: [Real] Real -> [Real]
normalizeList track peak = [x/safe_peak \\ x <- track]
where
	safe_peak | peak == 0.0 = 1.0 = peak;

renderAux :: [NoteChunk] -> [Real]
renderAux chunk_list = normalized
where
	total_samples = maxList [numberOfSamples x (x.note.initialTime+x.note.duration) \\ x <- chunk_list]
    rendered_track = [sum [renderIndex x chunk \\ chunk <- chunk_list] \\ x <-[1,2..total_samples]];
	normalized = normalizeList rendered_track (maxList [abs x \\ x <- rendered_track])

render :: [Note] ChannelProfile -> [Real]
render note_list chan_prof = renderAux chunk_list
where
	chunk_list = [noteToChunk nt chan_prof \\ nt <- note_list]

renderTotalSamplesAux :: [NoteChunk] -> Int
renderTotalSamplesAux chunk_list = total_samples
where
	total_samples = maxList [numberOfSamples x (x.note.initialTime+x.note.duration) \\ x <- chunk_list]

renderTotalSamples :: [Note] ChannelProfile -> Int
renderTotalSamples note_list chan_prof = renderTotalSamplesAux chunk_list
where
	chunk_list = [noteToChunk nt chan_prof \\ nt <- note_list]

totalRenderedAux :: [NoteChunk] -> Int
totalRenderedAux chunk_list = sum [numberOfSamples x x.note.duration \\ x <- chunk_list]

totalRendered :: [Note] ChannelProfile -> Int
totalRendered note_list chan_prof = totalRenderedAux chunk_list
where
	chunk_list = [noteToChunk nt chan_prof \\ nt <- note_list]

renderDataAux :: [NoteChunk] -> [(Int,Int)]
renderDataAux chunk_list = [((numberOfSamples x x.note.duration),(numberOfSamples x (x.note.initialTime+x.note.duration)))  \\ x <- chunk_list]

renderData :: [Note] ChannelProfile -> [(Int,Int)]
renderData note_list chan_prof = renderDataAux chunk_list
where
	chunk_list = [noteToChunk nt chan_prof \\ nt <- note_list]

noteToChunk :: Note ChannelProfile -> NoteChunk
noteToChunk nt chan_prof = {note = nt, wave = chan_prof.wavType, timeSig = nt.ts, tempo = nt.temp, dahdsr = chan_prof.envelope}
where
	ts = {barVal = 1, noteVal = 1}
	env = {delay = 0.0, attack = 1.0, hold = 0.0, decay = 2.0, sustain = 0.3, release = 1.0}
