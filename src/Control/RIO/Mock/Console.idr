module Control.RIO.Mock.Console

import Control.RIO
import Control.RIO.Console
import Data.IORef

%default total

||| A mock console where err out and std out are mutable refs
||| of snoc lists and `getChar` and `getLine` are simulated
||| via mutable refs of streams.
public export
record Mock where
  constructor MkMock
  stdOut : IORef (SnocList String)
  errOut : IORef (SnocList String)
  charIn : IORef (Stream Char)
  lineIn : IORef (Stream String)

||| Creates a mock console
export
mkMock : Stream Char -> Stream String -> IO Mock
mkMock cs ss =
  [| MkMock (newIORef [<]) (newIORef [<]) (newIORef cs) (newIORef ss) |]

getHead : IORef (Stream a) -> IO a
getHead ref = readIORef ref >>= \(h :: t) => writeIORef ref t $> h

||| A mock console, which uses the given streams for
||| simulating input, and the mutable refs for simulating
||| output.
export
console : Mock -> Console
console m =
  MkConsole (\s => modifyIORef m.stdOut (:< s))
            (\s => modifyIORef m.errOut (:< s))
            (getHead m.charIn)
            (getHead m.lineIn)
