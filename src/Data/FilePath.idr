module Data.FilePath

%default total

infixl 5 </>, />
infixr 7 <.>

public export
record FilePath where
  constructor MkFP
  path : String

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
