# musicmanage

A haskell program and a c# program, one which renames music based on id3 tags, to the form /artist/album/songname.extension, and another which removes disc numbers, and instead increments the track number.

You'll need taglib and taglib sharp installed, and the haskell modules mtl and taglib.

Place executables in /usr/bin/ or other location, then run fixmusic.sh in the desired folder.
