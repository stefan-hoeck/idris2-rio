module Main

import Control.RIO.Mock.File
import FSProps
import Hedgehog

%default total

main : IO ()
main = test [ FSProps.props ]
