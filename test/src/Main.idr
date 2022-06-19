module Main

import Control.RIO.Mock.File
import Hedgehog
import FilePathProps

%default total

main : IO ()
main = test [ FilePathProps.props ]
