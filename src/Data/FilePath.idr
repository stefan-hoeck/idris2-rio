module Data.FilePath

import Data.List1
import Data.String

%default total

--------------------------------------------------------------------------------
--          FilePath
--------------------------------------------------------------------------------

infixl 5 </>, />
infixr 7 <.>

public export
record FilePath where
  constructor MkFP
  path : String

export
concatPath : List String -> FilePath
concatPath = MkFP . fastConcat . intersperse "/"

export
splitPath : FilePath -> List1 String
splitPath = split ('/' ==) . path

public export
(</>) : FilePath -> FilePath -> FilePath
MkFP "" </> fp      = fp
fp      </> MkFP "" = fp
MkFP x  </> MkFP y  = MkFP "\{x}/\{y}"

public export
(/>) : FilePath -> String -> FilePath
fp /> s = fp </> MkFP s

public export
(<.>) : FilePath -> String -> FilePath
fp <.> s = MkFP $ "\{fp.path}.s"

public export
root : FilePath
root = MkFP "/"

export
isAbsolute : FilePath -> Bool
isAbsolute = isPrefixOf "/" . path

export
parentDir : FilePath -> Maybe FilePath
parentDir fp = case splitPath fp of
  _ ::: [] => Nothing
  x ::: ps => Just $ concatPath (init (x :: ps))

export
parentDirs : FilePath -> List FilePath
parentDirs fp = case splitPath fp of
  _ ::: [] => []
  x ::: ps => go [] (MkFP "") (init $ x :: ps)
    where go : List FilePath -> FilePath -> List String -> List FilePath
          go ps _  []        = ps
          go ps fp (x :: xs) = let fp2 = fp /> x in go (fp2 :: ps) fp2 xs

--------------------------------------------------------------------------------
--          Interfaces
--------------------------------------------------------------------------------

public export %inline
FromString FilePath where fromString = MkFP

export
Show FilePath where
  show = show . path

export
Interpolation FilePath where
  interpolate = path

public export
Eq FilePath where (==) = (==) `on` path

public export
Ord FilePath where compare = compare `on` path

public export
Semigroup FilePath where (<+>) = (</>)

public export
Monoid FilePath where neutral = ""
