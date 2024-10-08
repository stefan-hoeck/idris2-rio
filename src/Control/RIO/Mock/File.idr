module Control.RIO.Mock.File

import Control.RIO.App
import Control.RIO.File
import Data.FilePath
import Data.IORef
import Data.List1
import Data.Maybe
import System.File

%default total

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

public export
record Ctxt k v where
  constructor MkCtxt
  start : SnocList (k,v)
  key   : k
  end   : List (k,v)

export
unFocus : Ctxt k v -> v -> List (k,v)
unFocus (MkCtxt sp n ps) x = sp <>> ((n,x) :: ps)

export
focus : Eq k => k -> List (k,v) -> Maybe (Ctxt k v, v)
focus key = go [<]

  where
    go : SnocList (k,v) -> List (k,v) -> Maybe (Ctxt k v, v)
    go sx []        = Nothing
    go sx (x :: xs) = case fst x == key of
      True  => Just (MkCtxt sx (fst x) xs, snd x)
      False => go (sx :< x) xs

--------------------------------------------------------------------------------
--          Mock FS
--------------------------------------------------------------------------------

||| File or directory in a mock file system.
public export
data MockFile : Type where
  Regular : String -> MockFile
  Stream  : Stream String -> MockFile

public export
record MockDir where
  constructor MkMD
  content : List (Body, Either MockFile MockDir)

public export
0 AFile : Type
AFile = Either MockFile MockDir

public export
data Focus : Type where
  FileF :
       MockFile
    -> Ctxt Body AFile
    -> SnocList (Ctxt Body AFile)
    -> Focus
  DirF :
       MockDir
    -> SnocList (Ctxt Body AFile)
    -> Focus

public export
data PCFocus : Type where
  Parent :
       (child     : Body)
    -> (parendDir : MockDir)
    -> (context   : SnocList (Ctxt Body AFile))
    -> PCFocus
  Exists :  Focus -> PCFocus

selfOrParent : Path t -> Path t
selfOrParent fp = maybe fp fst $ split fp

export
mkdirFocus : MockDir -> Path Abs -> Maybe Focus
mkdirFocus (MkMD ps) (PAbs u sx) = go [<] ps (sx <>> [])

  where
    go :
         SnocList (Ctxt Body AFile)
      -> List (Body,AFile)
      -> List Body
      -> Maybe Focus
    go sx ps []        = Just $ DirF (MkMD ps) sx
    go sx ps (x :: xs) = case focus x ps of
      Nothing          =>
        let c := MkCtxt Lin x Nil
         in go (sx :< c) [] xs
      Just (c, Left f) => case xs of
        Nil => Just $ FileF f c sx
        _   => Nothing
      Just (c, Right $ MkMD ps2) => go (sx :< c) ps2 xs

export
dirFocus : MockDir -> Path Abs -> Maybe Focus
dirFocus (MkMD ps) (PAbs u sx) = go [<] ps (sx <>> [])

  where
    go :
         SnocList (Ctxt Body AFile)
      -> List (Body,AFile)
      -> List Body
      -> Maybe Focus
    go sx ps []        = Just $ DirF (MkMD ps) sx
    go sx ps (x :: xs) = case focus x ps of
      Nothing          => Nothing
      Just (c, Left f) => case xs of
        Nil => Just $ FileF f c sx
        _   => Nothing
      Just (c, Right $ MkMD ps2) => go (sx :< c) ps2 xs

unDirFocus' :
     List (Body, AFile)
  -> SnocList (Ctxt Body AFile)
  -> MockDir
unDirFocus' xs [<]       = MkMD xs
unDirFocus' xs (sx :< x) = unDirFocus' (unFocus x . Right $ MkMD xs) sx

export
unDirFocus : Focus -> MockDir
unDirFocus (FileF x c sx) = unDirFocus' (unFocus c (Left x)) sx
unDirFocus (DirF x sx)    = unDirFocus' x.content sx

||| A mock file system used for running simulations.
public export
record MockFS where
  constructor MkMockFS
  root   : MockDir
  curDir : Path Abs

absPath : MockFS -> Path t -> Path Abs
absPath fs (PAbs u sx) = PAbs u sx
absPath fs (PRel sx)   = fs.curDir </> PRel sx

export
fsFocus : MockFS -> Path t -> Maybe Focus
fsFocus fs = dirFocus fs.root . absPath fs

pcFocus : MockFS -> Path t -> Maybe PCFocus
pcFocus fs fp = case fsFocus fs fp of
  Just f  => Just (Exists f)
  Nothing => case split fp of
    Just (p,c) => case fsFocus fs p of
      Just (DirF d sx) => Just $ Parent c d sx
      _                => Nothing
    Nothing    => Nothing

export
exists : Path t -> MockFS -> Bool
exists fp fs = isJust $ fsFocus fs fp

export
mkDir : Path t -> MockFS -> Either FileErr MockFS
mkDir fp fs = case pcFocus fs fp of
  Just (Parent c d sx) =>
    let empty := (c, Right $ MkMD [])
        dir   := unDirFocus' (empty :: d.content) sx
     in Right $ {root := dir} fs
  Just (Exists _)      => Left (MkDir fp FileExists)
  Nothing              => Left (MkDir (selfOrParent fp) FileNotFound)

export
mkDirP : Path t -> MockFS -> Either FileErr MockFS
mkDirP fp fs = case mkdirFocus fs.root (absPath fs fp) of
  Just (DirF x sx) => Right $ {root := unDirFocus' x.content sx} fs
  Just (FileF {})  => Left (MkDir fp FileExists)
  Nothing => Left (MkDir fp FileExists)

writeImpl :
     File t
  -> String
  -> (String -> String)
  -> MockFS -> Either FileErr MockFS
writeImpl fp s f fs = case pcFocus fs $ toPath fp of
  Just (Parent c d sx) =>
    let empty := (c, Left $ Regular s)
        dir   := unDirFocus' (empty :: d.content) sx
     in Right $ {root := dir} fs
  Just (Exists (FileF (Regular x) d sx)) =>
    let dir   := unDirFocus $ FileF (Regular $ f x) d sx
     in Right $ {root := dir} fs
  Just (Exists _) => Left (WriteErr fp FileExists)
  Nothing         => Left (WriteErr fp FileNotFound)

export
write : File t -> String -> MockFS -> Either FileErr MockFS
write fp s = writeImpl fp s (const s)

export
append : File t -> String -> MockFS -> Either FileErr MockFS
append fp s = writeImpl fp s (++ s)

export
removeFile : File t -> MockFS -> Either FileErr MockFS
removeFile fp fs = case fsFocus fs $ toPath fp of
  Just (FileF _ (MkCtxt start _ end) sx) =>
    Right $ {root := unDirFocus' (start <>> end) sx} fs
  Nothing             => Left $ DeleteErr (toPath fp) FileNotFound
  Just (DirF {})      => Left $ DeleteErr (toPath fp) FileNotFound

export
removeDir : Path t -> MockFS -> Either FileErr MockFS
removeDir fp fs = case fsFocus fs fp of
  Just (DirF (MkMD []) (sx :< (MkCtxt start _ end))) =>
    Right $ {root := unDirFocus' (start <>> end) sx} fs
  Just (DirF {})           => Right fs
  Just (FileF {})          => Left $ DeleteErr fp FileNotFound
  Nothing                  => Left $ DeleteErr fp FileNotFound

export
read : File t -> Bits32 -> MockFS -> Either FileErr String
read fp ms fs = case fsFocus fs $ toPath fp of
  Just (FileF (Regular x) _ _)   => case length x <= cast ms of
    True  => Right x
    False => Left (LimitExceeded fp ms)
  Just (FileF (Stream _) _ _) => Right ""
  Just (DirF _ _) => Left (ReadErr fp FileReadError)
  Nothing  => Left (ReadErr fp FileNotFound)

export
changeDir : Path t -> MockFS -> Either FileErr MockFS
changeDir fp fs = case fsFocus fs fp of
  Just (DirF x sx)    => Right $ {curDir := absPath fs fp} fs
  Just (FileF x y sx) => Left (ChangeDir fp)
  Nothing             => Left (ChangeDir fp)

export
listDir : Path t -> MockFS -> (Either FileErr $ List Body)
listDir fp fs = case fsFocus fs fp of
  Just (DirF x _)    => Right $ map fst x.content
  Just (FileF _ _ _) => Left (ListDir fp FileReadError)
  Nothing            => Left (ListDir fp FileNotFound)

mock :  IORef MockFS
     -> (MockFS -> Either FileErr MockFS)
     -> IO (Either FileErr ())
mock ref f = do
  Right fs <- f <$> readIORef ref
    | Left err => pure (Left err)
  Right <$> writeIORef ref fs

||| A mock file system
export
fs : IORef MockFS -> FS
fs ref =
  MkFS
    { write_      = \_,fp,s => mock ref (write fp s)
    , append_     = \_,fp,s => mock ref (append fp s)
    , removeFile_ = \_,fp => mock ref (removeFile fp)
    , removeDir_  = \_,fp => mock ref (removeDir fp)
    , exists_     = \_,fp => exists fp <$> readIORef ref
    , read_       = \_,fp,l => read fp l <$> readIORef ref
    , curDir_     = Right . curDir <$> readIORef ref
    , changeDir_  = \_,fp => mock ref (changeDir fp)
    , listDir_    = \_,fp => listDir fp <$> readIORef ref
    , mkDir_      = \_,fp => mock ref (mkDir fp)
    }
