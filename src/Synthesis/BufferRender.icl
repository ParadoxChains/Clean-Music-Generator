implementation module Synthesis.BufferRender
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
= envelope * wave * (toReal chunk.note.veolocity)
where
	local_time = (global_time - (noteToSamples (convertDurToBeats chunk.note.initialTime chunk.timeSig) chunk.timeSig chunk.tempo))
    chunk_beats = (convertDurToBeats chunk.note.duration chunk.timeSig)
	sample_num = noteToSamples chunk_beats chunk.timeSig chunk.tempo
    wave = generateLocal local_time chunk.wave chunk.note.frequency
    envelope = getLocalDAHDSR local_time chunk_beats chunk.timeSig chunk.tempo chunk.dahdsr

renderBuffer :: Int Int [NoteChunk] -> [Real]
renderBuffer left right chunk_list = rendered_track
where
    rendered_track = [sum [renderIndex x chunk \\ chunk <- chunk_list] \\ x <-[left,(left+1)..right]];

inInterval :: Int Int NoteChunk -> Bool
inInterval left right chunk = (note_left <= right) && (note_right >= left)
where
    note_left = noteToSamples (convertDurToBeats chunk.note.initialTime chunk.timeSig) chunk.timeSig chunk.tempo
    note_right = numberOfSamples chunk (chunk.note.initialTime+chunk.note.duration)

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
    x = hd chunk_list
	bs = 8192
    total_samples = maxList [numberOfSamples x (x.note.initialTime+x.note.duration) \\ x <- chunk_list]
    rendered_track = flatten [renderBuffer (i*bs) (min ((i+1)*bs) total_samples) (filter (inInterval (i*bs) (min ((i+1)*bs) total_samples)) chunk_list) \\ i <- [0,1..(total_samples/bs)]]
    normalized = normalizeList rendered_track (maxList [abs x \\ x <- rendered_track])


render :: [Note] ChannelProfile -> [Real]
render note_list chan_prof = renderAux chunk_list
where
	chunk_list = [noteToChunk nt chan_prof \\ nt <- note_list]

noteToChunk :: Note ChannelProfile -> NoteChunk
noteToChunk nt chan_prof = {note = nt, wave = chan_prof.wavType, timeSig = nt.ts, tempo = nt.temp, dahdsr = chan_prof.envelope}
