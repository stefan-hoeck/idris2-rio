module Main

import Control.RIO.Mock.File
import FSProps
import FilePathProps
import Hedgehog

%default total

main : IO ()
main = test [ FilePathProps.props
            , FSProps.props
            ]
