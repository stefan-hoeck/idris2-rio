module Control.RIO.Mock.Console

import Control.RIO.Console
import Data.IORef

%default total

||| A mock output console where err out and std out are mutable refs
public export
record MockOut where
  constructor MkMockOut
  stdOut : IORef (SnocList String)
  errOut : IORef (SnocList String)

||| Creates a mock console
export
mkMockOut : IO MockOut
mkMockOut = [| MkMockOut (newIORef [<]) (newIORef [<]) |]

||| A mock output console, which uses the
||| the wrapped mutable refs for simulating
||| output.
export
consoleOut : MockOut -> ConsoleOut
consoleOut m =
  MkConsoleOut
    (\s => modifyIORef m.stdOut (:< s))
    (\s => modifyIORef m.errOut (:< s))

||| A mock console where err out and std out are mutable refs
||| of snoc lists and `getChar` and `getLine` are simulated
||| via mutable refs of streams.
public export
record MockIn where
  constructor MkMockIn
  charIn : IORef (Stream Char)
  lineIn : IORef (Stream String)

||| Creates a mock console
export
mkMockIn : Stream Char -> Stream String -> IO MockIn
mkMockIn cs ss = [| MkMockIn (newIORef cs) (newIORef ss) |]

getHead : IORef (Stream a) -> IO a
getHead ref = readIORef ref >>= \(h :: t) => writeIORef ref t $> h

||| A mock input console, which uses the given streams for
||| simulating input
export
console : MockIn -> ConsoleIn
console m =
  MkConsoleIn
    (getHead m.charIn)
    (getHead m.lineIn)
