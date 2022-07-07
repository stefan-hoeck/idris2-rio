module Control.RIO.File

import public Control.RIO.App
import public Data.FilePath.File
import public Data.DPair
import public Data.List
import Data.List1
import Data.IORef

import System.Directory
import System.File

%default total

--------------------------------------------------------------------------------
--          Error Type
--------------------------------------------------------------------------------

public export
data FileErr : Type where
  ReadErr       : (path : File t) -> (error : FileError) -> FileErr

  WriteErr      : (path : File t) -> (error : FileError) -> FileErr

  DeleteErr     : (path : Path t) -> (error : FileError) -> FileErr

  LimitExceeded : (path : File t) -> (limit : Bits32) -> FileErr

  CurDir        : FileErr

  ChangeDir     : Path t -> FileErr

  ListDir       : Path t -> FileError -> FileErr

  MkDir         : Path t -> FileError -> FileErr

export
printErr : FileErr -> String
printErr (ReadErr p err) =
  "Error when reading from \"\{p}\": \{show err}"
printErr (WriteErr p err) =
  "Error when writing to \"\{p}\": \{show err}"
printErr (DeleteErr p err) =
  "Error when deleteing \"\{p}\": \{show err}"
printErr (LimitExceeded p _) =
  "Error when reading \"\{p}\": File size limit exceeded."
printErr CurDir = "Failed to get current directory"
printErr (ChangeDir p) = "Failed to change to directory \"\{p}\""
printErr (ListDir p err) =
  "Error when reading directory \"\{p}\": \{show err}."
printErr (MkDir p err) =
  "Error when creating directory \"\{p}\": \{show err}."

--------------------------------------------------------------------------------
--          Record
--------------------------------------------------------------------------------

||| Record representing a simple file system, where we can
||| read from and write to files.
public export
record FS where
  constructor MkFS
  ||| Writes the string to the file in question
  write_  : (0 t : _) -> File t -> String -> IO (Either FileErr ())

  ||| Appends the string to the file in question
  append_ : (0 t : _) -> File t -> String -> IO (Either FileErr ())

  ||| Deletes the given file.
  removeFile_ : (0 t : _) -> File t -> IO (Either FileErr ())

  ||| Deletes the given directory.
  removeDir_ : (0 t : _) -> Path t -> IO (Either FileErr ())

  ||| Checks if the given file exists in the file system
  exists_ : (0 t : _) -> Path t -> IO Bool

  ||| This tries to read a file in whole, so we have to limit
  ||| the acceptable file size, otherwise this might overflow
  ||| the computer's memory if presented with an infinite stream
  ||| such as `/dev/zero`
  read_   : (0 t : _) -> File t -> Bits32 -> IO (Either FileErr String)

  ||| Prints the current working directory.
  curDir_ : IO (Either FileErr (Path Abs))

  ||| Change to the given directory
  changeDir_ : (0 t : _) -> Path t -> IO (Either FileErr ())

  ||| List entries in a directory (without `.` and `..`)
  listDir_ : (0 t : _) -> Path t -> IO (Either FileErr (List Body))

  ||| Creates the given directory
  mkDir_ : (0 t : _) -> Path t -> IO (Either FileErr ())

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

||| True if the given file or directory exists in the file system
export
exists : FS => Path t -> RIO x Bool
exists path = liftIO (exists_ %search t path)

||| True if the given file exists in the file system
export %inline
fileExists : FS => File t -> RIO x Bool
fileExists = exists . toPath

||| True if the given file or directory does not exist in the file system
export %inline
missing : FS => Path t -> RIO x Bool
missing = map not . exists

||| True if the given file does not exist in the file system
export %inline
fileMissing : FS => File t -> RIO x Bool
fileMissing = missing . toPath

||| Writes the given string to a file.
export
write : FS => Has FileErr xs => File t -> String -> App xs ()
write path str = injectIO (write_ %search t path str)

||| Writes the given string to a file.
export
append : FS => Has FileErr xs => File t -> String -> App xs ()
append path str = injectIO (append_ %search t path str)

||| Reads a string from a file, the size of which must not
||| exceed the given number of bytes.
export
read : FS => Has FileErr xs => File t -> Bits32 -> App xs String
read path limit = injectIO (read_ %search t path limit)

||| Delete a file from the file system.
export
removeFile : FS => Has FileErr xs => File t -> App xs ()
removeFile path = injectIO (removeFile_ %search t path)

||| Delete a file from the file system.
export
removeDir : FS => Has FileErr xs => Path t -> App xs ()
removeDir path = injectIO (removeDir_ %search t path)

||| Returns the current directory's path.
export
curDir : FS => Has FileErr xs => App xs (Path Abs)
curDir = injectIO $ curDir_ %search

||| Changes the working directory
export
chgDir : FS => Has FileErr xs => (dir : Path t) -> App xs ()
chgDir dir = injectIO (changeDir_ %search t dir)

||| Runs an action in the given directory, changing back
||| to the current directory afterwards.
export
inDir :  FS
      => Has FileErr xs
      => (dir : Path t)
      -> (act : App xs a)
      -> App xs a
inDir dir act = do
  cur <- curDir
  finally (chgDir cur) (chgDir dir >> act)

||| List entries in a directory (without `.` and `..`)
export
listDir : FS => Has FileErr xs => Path t -> App xs (List Body)
listDir dir = injectIO (listDir_ %search t dir)

||| Creates the given directory
export
mkDir : FS => Has FileErr xs => Path t -> App xs ()
mkDir dir = injectIO (mkDir_ %search t dir)

||| Creates the given directory (including parent directories)
export
mkDirP : FS => Has FileErr xs => Path t -> App xs ()
mkDirP dir = go (parentDirs dir) >> mkDir dir
  where go : List (Path t) -> App xs ()
        go []        = pure ()
        go (x :: xs) = when !(missing x) $ go xs >> mkDir x

||| Creates the parent directory of the given file
export %inline
mkParentDir : FS => Has FileErr xs => File t -> App xs ()
mkParentDir = mkDirP . parent

--------------------------------------------------------------------------------
--          Default FS
--------------------------------------------------------------------------------

writeImpl : (0 t : _) -> File t -> String -> IO (Either FileErr ())
writeImpl _ fp s = mapFst (WriteErr fp) <$> writeFile "\{fp}" s

appendImpl : (0 t : _) -> File t -> String -> IO (Either FileErr ())
appendImpl _ fp s = mapFst (WriteErr fp) <$> appendFile "\{fp}" s

removeFileImpl : (0 t : _) -> File t -> IO (Either FileErr ())
removeFileImpl _ fp = mapFst (DeleteErr $ toPath fp) <$> removeFile "\{fp}"

removeDirImpl : (0 t : _) -> Path t -> IO (Either FileErr ())
removeDirImpl _ fp = Right <$> removeDir "\{fp}"

readImpl : (0 t : _) -> File t -> Bits32 -> IO (Either FileErr String)
readImpl _ fp limit = do
  Right s <- withFile "\{fp}" Read pure fileSize
    | Left err => pure (Left $ ReadErr fp err)
  case s <= 0 of
    True => pure (Right "")
    False => case s <= cast limit of
      True  => assert_total $ do
        Left err <- readFile "\{fp}" | Right s => pure (Right s)
        pure (Left $ ReadErr fp err)

      False => pure (Left $ LimitExceeded fp limit)

curDirImpl : IO (Either FileErr (Path Abs))
curDirImpl = do
  Just s <- currentDir | Nothing => pure (Left CurDir)
  case fromString {ty = FilePath} s of
    (FP p@(PAbs {})) => pure (Right p)
    _                => pure (Left CurDir)

changeDirImpl : (0 t : _) -> Path t -> IO (Either FileErr ())
changeDirImpl _ dir = do
  True <- changeDir "\{dir}" | False => pure (Left $ ChangeDir dir)
  pure $ Right ()

listDirImpl : (0 t : _) -> Path t -> IO (Either FileErr $ List Body)
listDirImpl _ dir = bimap (ListDir dir) (mapMaybe parse) <$> listDir "\{dir}"

mkDirImpl : (0 t : _) -> Path t -> IO (Either FileErr ())
mkDirImpl _ dir = mapFst (MkDir dir) <$> createDir "\{dir}"

||| A computer's local file system
export
local : FS
local = MkFS {
    write_      = writeImpl
  , append_     = appendImpl
  , removeFile_ = removeFileImpl
  , removeDir_  = removeDirImpl
  , exists_     = \_,fp => exists "\{fp}"
  , read_       = readImpl
  , curDir_     = curDirImpl
  , changeDir_  = changeDirImpl
  , listDir_    = listDirImpl
  , mkDir_      = mkDirImpl
  }
