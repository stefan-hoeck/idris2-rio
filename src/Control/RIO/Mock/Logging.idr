module Control.RIO.Mock.Logging

import Control.RIO
import Control.RIO.Logging
import Data.IORef

%default total

||| A mock logging utility.
public export
record Mock where
  constructor MkMock
  ref : IORef (SnocList (LogLevel, Lazy String))

||| Creates a mock logging facility
export
mkMock : IO Mock
mkMock = MkMock <$> newIORef [<]

export
logging : Mock -> Logger_
logging m = MkLogger $ \l,s => modifyIORef m.ref (:< (l,s))
