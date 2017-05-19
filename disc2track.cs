using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using AlbumTags = System.Collections.Generic.Dictionary<string, 
                  System.Collections.Generic.List<TagLib.File>>;
using TagLib;

public class disc2track
{
    public static void Main()
    {
        var albums = GroupByAlbum(GetFiles()
                     .Where(x => Path.GetExtension(x) == ".mp3")
                     .Select(x => TagLib.File.Create(x))
                     .Where(x => x.Tag.Disc > 0).ToList());

        albums.Select(x => x.OrderBy(y => y.Tag.Disc).ThenBy(y => y.Tag.Track))
              .ToList().ForEach(x => DiscsToTracks(x.ToList()));

    }

    private static string[] GetFiles()
    {
        return Directory.GetFiles(".", "*.*", SearchOption.AllDirectories);
    }

    private static List<List<TagLib.File>> GroupByAlbum(List<TagLib.File> 
                                                        tagFiles)
    {
        AlbumTags albums = new AlbumTags();

        foreach (var file in tagFiles)
        {
            var album = file.Tag.Album;

            if (albums.ContainsKey(album))
            {
               albums[album].Add(file);
            }
            else
            {
                List<TagLib.File> tmp = new List<TagLib.File>();
                tmp.Add(file);
                albums.Add(album, tmp);
            }
        }

        return albums.Values.ToList();
    }

    private static void DiscsToTracks(IEnumerable<TagLib.File> files)
    {
        uint counter = 1;

        // list has been sorted already
        foreach (var file in files)
        {
            file.Tag.Track = counter++;
            file.Tag.Disc = 0;
            file.Save();
        }
    }
}
