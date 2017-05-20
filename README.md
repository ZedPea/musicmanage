# musicmanage

musicmanage.hs renames files based on id3 tags, to the form /artist/album/songname.extension.
This is good for when you download files and they're all messed up, often with the track number in front, inconsistent folder structure etc. Fix the tags with easytag for example, then run this program.

disc2track.cs removes disc numbers, and instead makes the tracks proceed after the next disk. So, if the first disc had 10 songs, the first song on disc two would become track 11.
This is good for when you have a program which doesn't support disc numbers, so instead plays track one disc one, track one disc two. Once running this it should work.

numberedfiles.hs is a slight modification of musicmanage. It renames them in the form /artist/album/tracknumber - songname.extension.
This is good for when you have a program which doesn't support track numbers, so instead plays songs in alphabetic order, which if you like your albums is very annoying. This should fix it to play in alphabetical order.

fixmusic runs musicmanage then disc2track.exe.

#### Installation

##### Install prerequisites

Install stack, taglib and taglib-sharp.

###### Arch based

`sudo pacman -S taglib taglib-sharp stack`

###### Debian based 

`sudo apt-get install taglib taglib-sharp haskell-stack`

##### Setup

`./setup`

You might need to run `chmod +x setup` if the above command doesn't work.

Add ~/.local/bin/ to your path by editing ~/.profile or manually editing your path variable

#### Running

run `fixmusic`, `musicmanage`, `numberedfiles` or `disc2track.exe` in the folder with your music in, depending upon what functionality you need.
