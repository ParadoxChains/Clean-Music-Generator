# Reading Resources
Here will be a nice little list of reading materials with background knowledge required for this project. This will be expanded upon as we find out more stuff we need to learn.

## Clean
Since we will be implementing with Clean, it might be helpful to reference things now and then.
- [A Functionl Programmer's Workout](https://github.com/ParadoxChains/BScFunctionalProgramming-2019-Fall/blob/master/Resources/A%20Functional%20Programmers%20Workout.pdf)
- [Functional Programming in Clean](https://github.com/ParadoxChains/BScFunctionalProgramming-2019-Fall/blob/master/Resources/CleanBookI.pdf)
  - This book has a nice section on reading from and writing to files, along with interaction with the outside world.
- [Functional Programming and Graph Rewriting](https://clean.cs.ru.nl/Functional_Programming_and_Parallel_Graph_Rewriting)

## Sound Theory
We're going to be playing with sound and generating it, so it's good to understand basics of sound theory.
- [Harmonics \[Wikipedia\]](https://en.wikipedia.org/wiki/Harmonic)
  - Harmonics are the basic building blocks of any wave type from simple sine waves. Understanding them will be necessary for synthesis.
- [Harmonics of a Square Wave \[All About Circuits\]](https://www.allaboutcircuits.com/textbook/alternating-current/chpt-7/square-wave-signals/)
  - A nice article going into excruciating detail of how a square wave is built.
- [Summary of various wave types \[Teach Me Audio\]](https://www.teachmeaudio.com/recording/sound-reproduction/common-waveshapes/)
  - A nice summary of the different wave types.
- More Wikipedia links to each of the different wavetypes we should be able to generate in order to be able to synthesize anything.
  - [Sine Wave](https://en.wikipedia.org/wiki/Sine_wave)
    - The basic building block for everything else
  - [Square Wave](https://en.wikipedia.org/wiki/Square_wave)
  - [Pulse Wave](https://en.wikipedia.org/wiki/Pulse_wave)
  - [Triangle Wave](https://en.wikipedia.org/wiki/Triangle_wave)
  - [Sawtooth Wave](https://en.wikipedia.org/wiki/Sawtooth_wave)
  - [Various Noise Types](https://en.wikipedia.org/wiki/Colors_of_noise)
- Synthesis 101. This is a good set of articles briefly covering various aspects of sound synthesis from a purely analog aspect. I will only post the ones relevant to what we will do.
  - [In Search of the Perfect Wave](https://en.audiofanzine.com/getting-started/editorial/articles/in-search-of-the-perfect-wave.html)
  - [Make Some Noise!](https://en.audiofanzine.com/sound-synthesis/editorial/articles/make-some-noise.html)
  - [Filter It Out!](https://en.audiofanzine.com/sound-synthesis/editorial/articles/filter-it-out.html)
    - This is a possible point for expansion in our project.
  - [Send It in an Envelope](https://en.audiofanzine.com/sound-synthesis/editorial/articles/send-it-in-an-envelope.html)
    - Another possible, and honestly a fairly simple expansion for our project.

## Digital Signal Processing
Aside from knowing basic sound theory, we need to understand the basics of digital signal processing, as it is very different from doing it analog.
- [Sample Rates and Bit Depth in a Nutshell \[Mastering the Mix\]](https://www.masteringthemix.com/blogs/learn/113159685-sample-rates-and-bit-depth-in-a-nutshell)
  - Good basic rundownon sampling rate + bit depth.
- [Sampling - Sampling Rate \[Wikipedia\]](https://en.wikipedia.org/wiki/Sampling_(signal_processing)#Sampling_rate)
  - A nice table of the various sampling rates commonly used and what they correspond to. We will be using the 44.1kHz standard.
- [Nyquist-Shannon sampling theorem](http://musicweb.ucsd.edu/~trsmyth/digitalAudio171/Nyquist_Sampling_Theorem.html)
  - A very complex theory in a very plain nutshell, no proofs or nothing. You can go to Wikipedia for all that fancy stuff.
- [WAV Format Briefly](http://soundfile.sapp.org/doc/WaveFormat/)
  - A nice short rticle about the .wav format which we will be using. Nice diagram is nice.
- [Demystifying the Wav Format \[Microsoft Developer\]](https://blogs.msdn.microsoft.com/dawate/2009/06/23/intro-to-audio-programming-part-2-demystifying-the-wav-format/)
  - Official blog from Microsoft about their own file format lol. But overall the best article so far on how to write .wav files.

## Others
Other stuff goes here, maybe I'll categorize them properly later.
- [Endianess](https://en.wikipedia.org/wiki/Endianness)
  - We will need to know this since the .wav file format is very specific on this.
- [Inspiration Code](https://codereview.stackexchange.com/questions/105272/writing-computer-generated-music-to-a-wav-file-in-c?fbclid=IwAR1cnRkABS7SPgRw8Y42T_tQQdTYVBx_B4JSp0KGCv3vafLlWpVej7Hw4do)
  - This C code is a nice base that we can reference off with our code.
  - [The final iteration](https://codereview.stackexchange.com/questions/106137/writing-computer-generated-music-to-a-wav-file-in-c-follow-up-2)
- 