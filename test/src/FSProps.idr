module FSProps

import Control.RIO.File
import Control.RIO.Mock.File
import Data.FilePath
import Data.Maybe
import Data.SOP
import Data.Vect
import Hedgehog

%default total

--------------------------------------------------------------------------------
--          Interfaces
--------------------------------------------------------------------------------

Show MockFile where
  showPrec p (Regular str) = showCon p "Regular" $ showArg str
  showPrec p (Stream xs)   = showCon p "Stream" $ showArg "..."

Show MockDir where
  showPrec p (MkMD c) = assert_total (showCon p "MkMD" $ showArg c)

Show MockFS where
  showPrec p (MkMockFS r c) = showCon p "MkMockFS" $ showArg r ++ showArg c

Eq FileErr where _ == _ = False

Show FileErr where show = printErr

--------------------------------------------------------------------------------
--          Generators
--------------------------------------------------------------------------------

bodyChar : Gen Char
bodyChar = frequency [(30, alphaNum), (1, element ['-', '_', '.'])]

body' : Gen String
body' = string (linear 1 20) bodyChar

body : Gen Body
body = fromMaybe "body" . parse <$> body'

export
ending : Gen Body
ending = fromMaybe "txt" . parse <$> string (linear 1 5) alphaNum

export
fileBody : Gen Body
fileBody = [| body <.> ending |]

export
relDir : Gen (Path Rel)
relDir = PRel . (Lin <><) <$> list (linear 0 6) body

export
absType : Gen AbsType
absType = choice [pure Unix, map Disk upper, [| UNC body' body' |]]

export
absDir : Gen (Path Abs)
absDir = [| PAbs absType pth |]
  where
    pth : Gen (SnocList Body)
    pth =  (Lin <><) <$> list (linear 0 6) body

export
dir : Gen FilePath
dir = choice [FP <$> absDir, FP <$> relDir]

toAF : FilePath -> Body -> AnyFile
toAF (FP p) b = AF $ MkF p b

export
file : Gen AnyFile
file = [| toAF dir fileBody |]

export
relativeFile : Gen (File Rel)
relativeFile = MkF neutral <$> fileBody

mockFS : Gen MockFS
mockFS = toFS (MkMockFS (MkMD []) root) <$> list (linear 0 20) dir

  where
    toFS : MockFS -> List FilePath -> MockFS
    toFS fs []        = fs
    toFS fs (FP p :: ps) = case mkDirP p fs of
      Right fs2 => toFS fs2 ps
      Left  _   => toFS fs ps

anyString : Gen String
anyString = string (linear 0 20) unicode

--------------------------------------------------------------------------------
--          Properties
--------------------------------------------------------------------------------

prop_rootExists : Property
prop_rootExists = property $ do
  fs <- forAll mockFS
  exists root fs === True

prop_readWrite : Property
prop_readWrite = property $ do
  [fs,fp,str] <- forAll $ np [mockFS,relativeFile,anyString]
  (write fp str fs >>= read fp 10000) === Right str

--------------------------------------------------------------------------------
--          Group
--------------------------------------------------------------------------------

export
props : Group
props =
  MkGroup
    "MockFS"
    [ ("prop_rootExists", prop_rootExists)
    , ("prop_readWrite", prop_readWrite)
    ]
