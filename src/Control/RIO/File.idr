module Control.RIO.File

import Control.RIO.App
import Data.FilePath
import System.File

%default total

--------------------------------------------------------------------------------
--          Error Type
--------------------------------------------------------------------------------

public export
data FileErr : Type where
  MkFileErr :  (mode  : FileMode)
            -> (path  : FilePath)
            -> (error : FileError)
            -> FileErr

  LimitExceeded : (path : FilePath) -> (limit : Bits32) -> FileErr

modeVerb : FileMode -> String
modeVerb Read    = "reading"
modeVerb Write   = "writing to"
modeVerb Execute = "executing"

export
printErr : FileErr -> String
printErr (MkFileErr m p err) =
  "Error when \{modeVerb m} \"\{p}\": \{show err}"
printErr (LimitExceeded p _) =
  "Error when reading \"\{p}\": File size limit exceeded."

--------------------------------------------------------------------------------
--          Record
--------------------------------------------------------------------------------

||| Record representing a simple file system, where we can
||| read from and write to files.
public export
record FS_ where
  constructor MkFS
  ||| Writes the string to the file in question
  write_  : FilePath -> String -> IO (Either FileErr ())

  ||| Appends the string to the file in question
  append_ : FilePath -> String -> IO (Either FileErr ())

  ||| Checks if the given file exists in the file system
  exists_ : FilePath -> IO Bool

  ||| This tries to read a file in whole, so we have to limit
  ||| the acceptable file size, otherwise this might overflow
  ||| the computer's memory if presented with an infinite stream
  ||| such as `/dev/zero`
  read_   : FilePath -> Bits32 -> IO (Either FileErr String)

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

||| Witness that the given environment `e` gives access
||| to a file system.
public export
interface FS (0 e : Type) where
  fs_ : e -> FS_

||| Get access to the file system from the environment.
export
fs : FS e => RIO e x FS_
fs = asks fs_

||| True if the given file exists in the file system
export
exists : FS e => FilePath -> RIO e x Bool
exists path = fs >>= \r => liftIO (exists_ r path)

||| Writes the given string to a file.
export
write : FS e => Has FileErr xs => FilePath -> String -> App e xs ()
write path str = fs >>= \r => injectIO (write_ r path str)

||| Writes the given string to a file.
export
append : FS e => Has FileErr xs => FilePath -> String -> App e xs ()
append path str = fs >>= \r => injectIO (append_ r path str)

||| Reads a string from a file, the size of which must not
||| exceed the given number of bytes.
export
read : FS e => Has FileErr xs => FilePath -> Bits32 -> App e xs String
read path limit = fs >>= \r => injectIO (read_ r path limit)

--------------------------------------------------------------------------------
--          Default FS
--------------------------------------------------------------------------------

writeImpl : FilePath -> String -> IO (Either FileErr ())
writeImpl fp s = mapFst (MkFileErr Write fp) <$> writeFile fp.path s

appendImpl : FilePath -> String -> IO (Either FileErr ())
appendImpl fp s = mapFst (MkFileErr Write fp) <$> appendFile fp.path s

readImpl : FilePath -> Bits32 -> IO (Either FileErr String)
readImpl fp limit = do
  Right s <- withFile fp.path Read pure fileSize
    | Left err => pure (Left $ MkFileErr Read fp err)
  case s <= 0 of
    True => pure (Right "")
    False => case s <= cast limit of
      True  => assert_total $ do
        Left err <- readFile fp.path | Right s => pure (Right s)
        pure (Left $ MkFileErr Read fp err)

      False => pure (Left $ LimitExceeded fp limit)

||| A computer's local file system
export
local : FS_
local = MkFS {
    write_  = writeImpl
  , append_ = appendImpl
  , exists_ = \fp => exists fp.path
  , read_   = readImpl
  }
