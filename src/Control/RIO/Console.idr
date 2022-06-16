module Control.RIO.Console

import Control.RIO
import Data.IORef
import System.File

%default total

--------------------------------------------------------------------------------
--          Record
--------------------------------------------------------------------------------

||| Record representing a console with
||| standard output, error output, and standard input.
public export
record Console_ where
  constructor MkConsole
  putStr_   : String -> IO ()
  putErr_   : String -> IO ()
  getChar_  : IO Char
  getLine_  : IO String

||| The default console, reading from standard input and printing
||| to standard out and standard err.
export
stdIO : Console_
stdIO = MkConsole putStr (\s => ignore $ fPutStr stderr s) getChar getLine

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

||| Witness that the given environment `e` gives access
||| to a console.
public export
interface Console (0 e : Type) where
  console_ : e -> Console_

||| Get access to the console from the environment.
export
console : Console e => RIO e x Console_
console = asks console_

||| Put a string to the console's standard output.
export
cputStr : Console e => String -> RIO e x ()
cputStr s = console >>= \c => liftIO $ c.putStr_ s

||| Put a string plus trailing line break
||| to the console's standard output.
export
cputStrLn : Console e => String -> RIO e x ()
cputStrLn s = cputStr $ s ++ "\n"

||| Print a value to the console's standard output.
export
cprint : Console e => Show a => a -> RIO e x ()
cprint = cputStr . show

||| Print a value plus trailing lne break
||| to the console's standard output.
export
cprintLn : Console e => Show a => a -> RIO e x ()
cprintLn = cputStrLn . show

||| Put a string to the console's error output.
export
cputErr : Console e => String -> RIO e x ()
cputErr s = console >>= \c => liftIO $ c.putErr_ s

||| Put a string plus trailing line break
||| to the console's error output.
export
cputErrLn : Console e => String -> RIO e x ()
cputErrLn s = cputErr $ s ++ "\n"

||| Print a value to the console's error output.
export
cprintErr : Console e => Show a => a -> RIO e x ()
cprintErr = cputErr . show

||| Print a value plus trailing lne break
||| to the console's error output.
export
cprintErrLn : Console e => Show a => a -> RIO e x ()
cprintErrLn = cputErrLn . show

||| Read a line from the console's standard input.
export
cgetLine : Console e => RIO e x String
cgetLine = console >>= \c => liftIO $ c.getLine_

||| Read a single character from the console's standard input.
export
cgetChar : Console e => RIO e x Char
cgetChar = console >>= \c => liftIO $ c.getChar_
