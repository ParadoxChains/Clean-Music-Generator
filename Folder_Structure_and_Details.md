# Folder Structure and Details
----
## Brief Overview
### As we are now at the point in the project where each subgroup will need to interact with modules from the other subgroups, it is to our benefit to assemble all codes into a directory. This is the `src` subdirectory.
### Before moving your code, please edit your .dcl files to have proper documentation (via comments) and proper interface functions for others to use. You may need to refactor your code.
### Definition modules must be named as 'subdir.subdir.module' and imported as such.
### After the merging of files is complete, the old files will go into `archive`, for archival purposes.
### It must be **well emphasized** that pushes from here on out must be done via **Forking and Pull Requests**. This allows for conflict evaluation, and for group review before pushing to master. If you plan on making a pull request that touches someone else's module, please tag that person so they may review it as well. If you don't know how to do the forking and pull request process, here is [a nice short read about how to do so](https://gist.github.com/Chaser324/ce0505fbed06b947d962): [https://gist.github.com/Chaser324/ce0505fbed06b947d962](https://gist.github.com/Chaser324/ce0505fbed06b947d962).
----
## Folder Structure
- bin *Final binaries will be placed here.*
- src *Source code goes into here.*
  - input *Code related to reading input will go here.*
  - output *Code relating to writing output will go here.*
  - voices *Code relating to generating synth voices go here.*
  - synthesis *Code for synthesizing waveforms will go here.*
  - util *Utilities and other necessary internal libraries can go here.*
  - lib *External libraries, if any, will be saved here.*
- test_files *All testing related files go here.*
  - bin *Test builds go here.*
  - icl *Test specific code goes here.*
  - input *Test input files will go in here.*
    - MIDI *Test midi input files.*
  - output *Test output files go here.*