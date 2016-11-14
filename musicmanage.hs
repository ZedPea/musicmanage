import qualified Sound.TagLib as TagLib
import Control.Monad (when)
import System.Directory
import System.FilePath (takeExtension, takeDirectory, (</>), (<.>), joinPath)
import Control.Exception (catch)
import Text.Printf (printf)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, execWriterT)

data SongInfo = SongInfo { 
    artist :: String,
    album :: String,
    title :: String
}

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    foo <- filter isSong <$> execWriterT (getFiles cwd)
    let files = filter isSong foo
    mapM_ getSongInfoAndRename files
    cleanUp cwd

getSongInfoAndRename :: FilePath -> IO ()
getSongInfoAndRename path = do
    cwd <- getCurrentDirectory
    maybeTagFiles <- runMaybeT $ getTagFiles path
    case maybeTagFiles of
        Nothing -> return ()
        Just files -> do
        info <- getSongInfo files
        case getNewPathIfPossible path cwd info of
            Just newpath -> renameSong path newpath
            Nothing -> return ()

getFiles :: FilePath -> WriterT [FilePath] IO ()
getFiles path = do
    isDir <- liftIO . doesDirectoryExist $ path
    if isDir
        then do
        files <- liftIO $ listDirectory path
        let paths = map (path </>) files
        mapM_ getFiles paths
    else tell [path]

--get a tag and tagfile out of the annoying wrapping if possible
getTagFiles :: FilePath -> MaybeT IO (TagLib.Tag, TagLib.TagFile)
getTagFiles path = do
    maybeTagFile <- MaybeT $ TagLib.open path
    maybeTag <- MaybeT $ TagLib.tag maybeTagFile
    return (maybeTag, maybeTagFile)

getNewPathIfPossible :: FilePath -> FilePath -> SongInfo -> Maybe FilePath
getNewPathIfPossible path cwd info
    | areAnyEmpty info || path == newpath = Nothing
    | otherwise = Just newpath
    where newpath = joinPath components <.> extension
          components = [cwd,rs $ artist info,rs $ album info,rs $ title info]
          rs = removeSlash
          extension = takeExtension path

renameSong :: FilePath -> FilePath -> IO ()
renameSong path newpath = do
    printf "Renaming %s to %s\n" path newpath
    createDirectoryIfMissing True $ takeDirectory newpath
    renameFile path newpath `catch` handler

cleanUp :: FilePath -> IO ()
cleanUp path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            files <- listDirectory path
            let paths = map (path </>) files
            mapM_ cleanUp paths
            newfiles <- listDirectory path
            when (null newfiles) $ removeDirectory path
        else when (takeExtension path `elem` canDelete) $ removeFile path

getSongInfo :: (TagLib.Tag,TagLib.TagFile) -> IO SongInfo
getSongInfo (t,tagfile) = do
    artist' <- TagLib.artist t
    album' <- TagLib.album t
    title' <- TagLib.title t
    TagLib.save tagfile
    return (SongInfo artist' album' title')

handler :: IOError -> IO ()
handler e = putStr "Error renaming file: " >> print e

-- '/' will mess up the desired paths
removeSlash :: String -> String
removeSlash = map f
    where f '/' = '-'
          f c = c

--make it easy to expand to other filetypes
filetypes :: [String]
filetypes = [".mp3", ".flac"]

--make it a bit harder for user to delete all their important files...
canDelete :: [String] 
canDelete = [".jpg", ".png", ".txt", ".nfo", ".jpeg", ".cue", ".log"]

areAnyEmpty :: SongInfo -> Bool
areAnyEmpty info = any null [artist info, album info, title info]

isSong :: FilePath -> Bool
isSong path = takeExtension path `elem` filetypes
