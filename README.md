# musicmanage

musicmanage.hs renames files based on id3 tags, to the form /artist/album/songname.extension.
This is good for when you download files and they're all messed up, often with the track number in front, inconsistent folder structure etc. Fix the tags with easytag for example, then run this program.

disc2track.cs removes disc numbers, and instead makes the tracks proceed after the next disk. So, if the first disc had 10 songs, the first song on disc two would become track 11.
This is good for when you have a program which doesn't support disc numbers, so instead plays track one disc one, track one disc two. Once running this it should work.

numberedfiles.hs is a slight modification of musicmanage. It renames them in the form /artist/album/tracknumber - songname.extension.
This is good for when you have a program which doesn't support track numbers, so instead plays songs in alphabetic order, which if you like your albums is very annoying. This should fix it to play in alphabetical order.

fixmusic runs musicmanage then disc2track.exe.

Both programs need the taglib library installed

musicmanage and numberedfiles need the taglib and mtl modules installed, from cabal, or other.

disc2track needs taglib-sharp downloaded, using a dll and passing it in when you compile is easiest.

Place all the files in /usr/bin/ or whatever your default location your distro has for executables.

You can also place them in the local directories, but it's more of a pain as you have to move them all to the desired location folder.

Once installed, run fixmusic / ./fixmusic in the desired folder, or a single script if that's all you need.
