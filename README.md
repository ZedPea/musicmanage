# musicmanage

Renames files based on ID3 tags, in the form /artist/album/songname

Usage: Run in the directory you want to rename the files of.

Note: This program deletes image files, text files, cue files and nfo files. Make sure you run it in a folder which just has music in!

It is designed to rename music which has been freshly downloaded, which often is named for example, file01.mp3, file02.mp3, etc, but has valid tags, to their correct names and folders. It then deletes album art, cue files, and text descriptions, along with any empty folders.

It would be trivial to extend this program to rename in user defined configurations, but I only ever rename in the form /artist/album/songname

## Installation:

#### Clone the repository
`git clone https://github.com/ZedPea/musicmanage.git`

#### Install dependencies
You must have the haskell platform including cabal installed.

`cabal install taglib`

#### Compile
Enter the directory containing musicmanage.hs and run

`ghc musicmanage.hs`

Note that some intermediate compile files will be left around, musicmanage.hi, and musicamanage.o.
You can delete these if you wish.

#### Running the program
Run 

./musicmanage in your music directory.
