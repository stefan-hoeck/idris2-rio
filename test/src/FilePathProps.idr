module FilePathProps

import Data.FilePath
import Data.SOP
import Data.Vect
import Hedgehog

%default total

--------------------------------------------------------------------------------
--          Generators
--------------------------------------------------------------------------------

fpChar : Gen Char
fpChar = frequency [(30, alphaNum), (1, element ['-', '_'])]

basename : Gen String
basename = string (linear 1 20) fpChar

export
ending : Gen String
ending = string (linear 1 5) alphaNum

export
relDir : Gen (Path Rel)
relDir = PRel . (Lin <><) <$> list (linear 0 6) basename

export
absDir : Gen (Path Abs)
absDir = PAbs . (Lin <><) <$> list (linear 0 6) basename

export
dir : Gen FilePath
dir = choice [FP <$> absDir, FP <$> relDir]

export
file : Gen FilePath
file = [| dir <.> ending |]

export
relativeFile : Gen FilePath
relativeFile = [| (fromString <$> basename) <.> ending |]

--------------------------------------------------------------------------------
--          Properties
--------------------------------------------------------------------------------

prop_split : Property
prop_split = property $ do
  [d,n] <- forAll $ np [dir,basename]
  split (d /> n) === Just (d,n)

prop_splitFile : Property
prop_splitFile = property $ do
  [d,n,e] <- forAll $ np [dir,basename,ending]
  split ((d /> n) <.> e) === Just (d,"\{n}.\{e}")

prop_roundtrip : Property
prop_roundtrip = property $ do
  f <- forAll file
  fromString (show f) === f

prop_prependEmpty : Property
prop_prependEmpty = property $ do
  d <- forAll relDir
  ("" </> d) === FP d

prop_appendEmpty : Property
prop_appendEmpty = property $ do
  f <- forAll file
  (f /> "") === f

prop_prependEmpty2 : Property
prop_prependEmpty2 = property $ do
  d <- forAll basename
  ("" /> d) === fromString d

--------------------------------------------------------------------------------
--          Group
--------------------------------------------------------------------------------

export
props : Group
props = MkGroup "FilePath" [
        ("prop_split", prop_split)
      , ("prop_splitFile", prop_splitFile)
      , ("prop_roundtrip", prop_roundtrip)
      , ("prop_prependEmpty", prop_prependEmpty)
      , ("prop_prependEmpty2", prop_prependEmpty2)
      , ("prop_appendEmpty", prop_appendEmpty)
      ]
