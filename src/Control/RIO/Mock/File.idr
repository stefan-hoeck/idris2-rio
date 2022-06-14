module Control.RIO.Mock.File

import Control.RIO.App
import Control.RIO.File
import Data.FilePath
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
  where go : SnocList (k,v) -> List (k,v) -> Maybe (Ctxt k v, v)
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
  content : List (String, Either MockFile MockDir)

public export
0 AnyFile : Type
AnyFile = Either MockFile MockDir

public export
data Focus : Type where
  FileF :  MockFile
        -> Ctxt String AnyFile
        -> SnocList (Ctxt String AnyFile)
        -> Focus
  DirF  :  MockDir
        -> SnocList (Ctxt String AnyFile)
        -> Focus

public export
data PCFocus : Type where
  Parent :  (child     : String)
         -> (parendDir : MockDir)
         -> (context   : SnocList (Ctxt String AnyFile))
         -> PCFocus
  Exists :  Focus -> PCFocus 

selfOrParent : FilePath -> FilePath
selfOrParent fp = maybe fp fst $ split fp

export
dirFocus : MockDir -> Path Abs -> Maybe Focus
dirFocus (MkMD ps) (PAbs sx) = go [<] ps (sx <>> [])
  where go :  SnocList (Ctxt String AnyFile)
           -> List (String,AnyFile)
           -> List String
           -> Maybe Focus
        go sx ps []        = Just $ DirF (MkMD ps) sx
        go sx ps (x :: xs) = case focus x ps of
          Nothing          => Nothing
          Just (c, Left f)           => case xs of
            Nil => Just $ FileF f c sx
            _   => Nothing
          Just (c, Right $ MkMD ps2) => go (sx :< c) ps2 xs

unDirFocus' :  List (String, AnyFile)
            -> SnocList (Ctxt String AnyFile)
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

absPath : MockFS -> FilePath -> Path Abs
absPath fs (FP $ PAbs sx) = PAbs sx
absPath fs (FP $ PRel sx) = fs.curDir </> PRel sx

export
fsFocus : MockFS -> FilePath -> Maybe Focus
fsFocus fs = dirFocus fs.root . absPath fs

pcFocus : MockFS -> FilePath -> Maybe PCFocus
pcFocus fs fp = case fsFocus fs fp of
  Just f  => Just (Exists f)
  Nothing => case split fp of
    Just (p,c) => case fsFocus fs p of
      Just (DirF d sx) => Just $ Parent c d sx
      _                => Nothing
    Nothing    => Nothing

export
exists : MockFS -> FilePath -> Bool
exists fs = isJust . fsFocus fs

export
mkDir : MockFS -> FilePath -> Either FileErr MockFS
mkDir fs fp = case pcFocus fs fp of
  Just (Parent c d sx) =>
    let empty := (c, Right $ MkMD [])
        dir   := unDirFocus' (empty :: d.content) sx
     in Right $ {root := dir} fs
  Just (Exists _)      => Left (MkDir fp FileExists)
  Nothing              => Left (MkDir (selfOrParent fp) FileNotFound)

writeImpl :  FilePath
          -> String
          -> (String -> String)
          -> MockFS -> Either FileErr MockFS
writeImpl fp s f fs = case pcFocus fs fp of
  Just (Parent c d sx) =>
    let empty := (c, Left $ Regular s)
        dir   := unDirFocus' (empty :: d.content) sx
     in Right $ {root := dir} fs
  Just (Exists (FileF (Regular x) d sx)) =>
    let dir   := unDirFocus $ FileF (Regular $ f x) d sx
     in Right $ {root := dir} fs
  Just (Exists _) => Left (WriteErr (selfOrParent fp) FileExists)
  Nothing         => Left (WriteErr (selfOrParent fp) FileNotFound)

export
write : FilePath -> String -> MockFS -> Either FileErr MockFS
write fp s = writeImpl fp s (const s)

export
append : FilePath -> String -> MockFS -> Either FileErr MockFS
append fp s = writeImpl fp s (++ s)

export
read : FilePath -> Bits32 -> MockFS -> Either FileErr String
read fp ms fs = case fsFocus fs fp of
  Just (FileF (Regular x) _ _)   => case length x <= cast ms of
    True  => Right x
    False => Left (LimitExceeded fp ms)
  Just (FileF (Stream _) _ _) => Right ""
  Just (DirF _ _) => Left (ReadErr fp FileReadError)
  Nothing  => Left (ReadErr fp FileNotFound)

export
changeDir : FilePath -> MockFS -> Either FileErr MockFS
changeDir fp fs = case fsFocus fs fp of
  Just (DirF x sx)    => Right $ {curDir := absPath fs fp} fs
  Just (FileF x y sx) => Left (ChangeDir fp)
  Nothing             => Left (ChangeDir fp)

export
listDir : FilePath -> MockFS -> (Either FileErr $ List FilePath)
listDir fp fs = case fsFocus fs fp of
  Just (DirF x _)    => Right $ map (fromString . fst) x.content
  Just (FileF _ _ _) => Left (ListDir fp FileReadError)
  Nothing            => Left (ListDir fp FileNotFound)