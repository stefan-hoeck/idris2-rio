module FSProps

import Control.RIO.File
import Control.RIO.Mock.File
import Data.FilePath
import FilePathProps
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

--------------------------------------------------------------------------------
--          Generators
--------------------------------------------------------------------------------

fsGen : Gen MockFS
fsGen = toFS (MkMockFS (MkMD []) root) <$> list (linear 0 20) dir
  where toFS : MockFS -> List FilePath -> MockFS
        toFS fs []        = fs
        toFS fs (p :: ps) = case mkDirP p fs of
          Right fs2 => toFS fs2 ps
          Left  _   => toFS fs ps

prop_rootExists : Property
prop_rootExists = property $ do
  fs <- forAll fsGen
  exists root fs === True

--------------------------------------------------------------------------------
--          Group
--------------------------------------------------------------------------------

export
props : Group
props = MkGroup "MockFS" [
        ("prop_rootExists", prop_rootExists)
      ]
