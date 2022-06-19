module Data.FilePath

import Data.List1
import Data.String

%default total

--------------------------------------------------------------------------------
--          Path
--------------------------------------------------------------------------------

infixl 5 </>, />
infixr 7 <.>

||| A path in the file system is either relative
||| or absolute.
public export
data PathType = Rel | Abs

||| A path in the file system.
|||
||| Right now, only Unix-style paths are supported.
public export
data Path : PathType -> Type where
  PAbs : SnocList String -> Path Abs
  PRel : SnocList String -> Path Rel

||| Concatenate two paths, the second of which must be
||| relative.
public export
(</>) : Path t -> Path Rel -> Path t
(</>) (PAbs sx) (PRel sy) = PAbs (sx ++ sy)
(</>) (PRel sx) (PRel sy) = PRel (sx ++ sy)

||| Append a file or directory to a path.
public export
(/>) : Path t -> String -> Path t
fp /> s = fp </> PRel [< s]

||| Try and split a path into parent directory and
||| file/directory name.
public export
split : Path t -> Maybe (Path t, String)
split (PAbs (sx :< x)) = Just (PAbs sx, x)
split (PRel (sx :< x)) = Just (PRel sx, x)
split (PAbs [<])       = Nothing
split (PRel [<])       = Nothing

||| Append a file ending to a path. If the path is empty,
||| this appends a hidden file/directory by prepending the
||| name with a dot.
public export
(<.>) : Path t -> String -> Path t
PAbs (sx :< x) <.> s = PAbs (sx :< "\{x}.\{s}")
PRel (sx :< x) <.> s = PRel (sx :< "\{x}.\{s}")
PRel [<]       <.> s = PRel [< ".\{s}"]
PAbs [<]       <.> s = PAbs [< ".\{s}"]

||| The root of the file system.
public export
root : Path Abs
root = PAbs [<]

||| Checks whether an unknown path is absolute or not.
export
isAbsolute : Path t -> Bool
isAbsolute (PAbs _) = True
isAbsolute (PRel _) = False

||| Tries to extract the parent directory from a path.
export
parentDir : Path t -> Maybe (Path t)
parentDir = map fst . split

||| Returns a list of parent directories of the given path.
export
parentDirs : Path t -> List (Path t)
parentDirs fp = case parentDir fp of
  Nothing => []
  Just p  => p :: parentDirs (assert_smaller fp p)

--------------------------------------------------------------------------------
--          Interfaces
--------------------------------------------------------------------------------

export
Show (Path t) where
  show (PAbs sx) = fastConcat . ("/" ::) . intersperse "/" $ sx <>> []
  show (PRel sx) = fastConcat . intersperse "/" $ sx <>> []

export
Interpolation (Path t) where
  interpolate = show

export
heq : Path t1 -> Path t2 -> Bool
heq (PAbs sx) (PAbs sy) = sx == sy
heq (PRel sx) (PRel sy) = sx == sy
heq _         _         = False

export
hcomp : Path t1 -> Path t2 -> Ordering
hcomp (PAbs sx) (PAbs sy) = compare sx sy
hcomp (PRel sx) (PRel sy) = compare sx sy
hcomp (PAbs _)  (PRel _)  = LT
hcomp (PRel _)  (PAbs _)  = GT

public export %inline
Eq (Path t) where (==) = heq

public export
Ord (Path t) where compare = hcomp

public export
Semigroup (Path Rel) where (<+>) = (</>)

public export
Monoid (Path Rel) where neutral = PRel [<]

--------------------------------------------------------------------------------
--          FilePath
--------------------------------------------------------------------------------

||| A path (relative or absolute) in a file system.
public export
record FilePath where
  constructor FP
  {0 pathType : PathType}
  path        : Path pathType

public export %inline
Eq FilePath where
  FP p1 == FP p2 = heq p1 p2

public export
Ord FilePath where
  compare (FP p1) (FP p2) = hcomp p1 p2

export
Show FilePath where show (FP p) = show p

export
Interpolation FilePath where interpolate (FP p) = interpolate p

public export
FromString FilePath where
  fromString s = case split ('/' ==) s of
    "" ::: ps => FP $ PAbs $ [<]   <>< ps
    s  ::: ps => FP $ PRel $ [< s] <>< ps

namespace FilePath

  ||| Append a file or directory to a path.
  public export
  (/>) : FilePath -> String -> FilePath
  FP fp /> s = FP $ fp /> s

  ||| Try and split a path into parent directory and
  ||| file/directory name.
  public export
  split : FilePath -> Maybe (FilePath, String)
  split (FP p) = map (\(fp,s) => (FP fp, s)) $ split p

  ||| Append a file ending to a path. If the path is empty,
  ||| this appends a hidden file/directory by prepending the
  ||| name with a dot.
  public export
  (<.>) : FilePath -> String -> FilePath
  FP fp <.> s = FP $ fp <.> s

  ||| The root of the file system.
  public export
  root : FilePath
  root = FP $ PAbs [<]

  ||| Checks whether an unknown path is absolute or not.
  export
  isAbsolute : FilePath -> Bool
  isAbsolute (FP $ PAbs _) = True
  isAbsolute (FP $ PRel _) = False

  ||| Tries to extract the parent directory from a path.
  export
  parentDir : FilePath -> Maybe FilePath
  parentDir = map fst . split

  ||| Returns a list of parent directories of the given path.
  export
  parentDirs : FilePath -> List FilePath
  parentDirs (FP p) = map (\p' => FP p') $ parentDirs p
