module GpoM.FS where

import qualified Data.ByteString as BS
import System.PosixCompat.Types
import System.PosixCompat.Files
import System.FilePath.Posix hiding ((</>))
import System.Directory
import System.Fuse
import GpoM.Types
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Exception.Base
import Data.List (isPrefixOf)

type LogF = String -> IO ()
type HT = ()
type FSError = (Errno, String)

noErr :: FSError
noErr = (eOK, "")

data State = State{ hadfsPath :: !FilePath
                  , sysvolPath :: !FilePath
                  , mp :: !FilePath
                  , cachePath :: !FilePath
                  , logF :: !LogF
                  }

runFS :: FilePath -> FilePath -> FilePath -> FilePath -> LogF -> IO ()
runFS hadfsMp sysvolMp mountpoint cacheDir logF = do
  errorState <- newTVarIO noErr
  let st = State hadfsMp sysvolMp mountpoint cacheDir logF
      prog = "gpoM"
      lePath = cacheDir <//> ".lasterror"

  BS.writeFile lePath BS.empty
  fuseRun prog [mountpoint
               ,"-o", "default_permissions,auto_unmount"
--               ,"-o", "subtype=" ++ host, "-o", "fsname=" ++ prog
               ,"-o", "big_writes"
               ,"-f"
               ] (adFSOps st) (exceptionHandler errorState lePath)

adFSOps :: State -> FuseOperations HT
adFSOps s = defaultFuseOps { fuseGetFileStat = getFileStat s
                           , fuseOpen        = openF s
                           , fuseRead        = readF s
                           , fuseOpenDirectory = openD s
                           , fuseReadDirectory = readD s
                           , fuseGetFileSystemStats = getFileSystemStats s
                           -- create operations
                           , fuseCreateDirectory = mkdir s
                           , fuseCreateDevice = create s
                           -- delete operations
                           , fuseRemoveLink = unlink s
                           , fuseRemoveDirectory = rmdir s
                           -- write operations
                           , fuseSetFileSize = setFSize s
                           , fuseWrite = write s
                           -- other
                           , fuseSetFileTimes = setFTime s
                           , fuseRename = mv s
                           }

exceptionHandler :: TVar FSError -> FilePath -> (SomeException -> IO Errno)
exceptionHandler lerr lePath e = do
  atomically $ writeTVar lerr (eFAULT, show e)
  writeFile lePath (show e)
  putStrLn $ show e
  return eFAULT

getFileStat State{..} path = do
  status <- getFileStatus (cachePath <//> path)
  return $ Right $ fromStatus status

openF _ _ _ _ = return $ Right ()

readF State{..} path _ len off = do
  content <- BS.readFile (cachePath <//> path)
  return . Right . BS.take (fromIntegral len) . BS.drop (fromIntegral off) $ content

openD _ "/" = return eOK
openD State{..} path = return eOK

readD State{..} path = do
  content <- listDirectory (cachePath <//> path) >>= \x -> return $ filter (not . isPrefixOf "..") x
  stats <- mapM (\x -> fromStatus <$> getFileStatus (cachePath <//> path <//> x)) content
  return $ Right $ zip content stats

mkdir = undefined
create = undefined
unlink = undefined
rmdir = undefined
setFSize = undefined
write = undefined
setFTime = undefined
mv = undefined

fromStatus :: FileStatus -> FileStat
fromStatus st =
  FileStat { statEntryType = entryType st
           , statFileMode = fileMode st
           , statLinkCount = linkCount st
           , statFileOwner = fileOwner st
           , statFileGroup = fileGroup st
           , statSpecialDeviceID = specialDeviceID st
           , statFileSize = fileSize st
           , statBlocks = 1
           , statAccessTime = accessTime st
           , statModificationTime = modificationTime st
           , statStatusChangeTime = statusChangeTime st
           }

entryType :: FileStatus -> EntryType
entryType m | isDirectory m = Directory
            | otherwise = RegularFile

(<//>) :: FilePath -> FilePath -> FilePath
(<//>) x y | head y == '/' = x `combine` tail y
           | otherwise = x `combine` y
infixr 5 <//>

getFileSystemStats :: State -> String -> IO (Either Errno FileSystemStats)
getFileSystemStats State{..} str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
