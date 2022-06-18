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
record Console where
  constructor MkConsole
  putStr_   : String -> IO ()
  putErr_   : String -> IO ()
  getChar_  : IO Char
  getLine_  : IO String

||| The default console, reading from standard input and printing
||| to standard out and standard err.
export
stdIO : Console
stdIO = MkConsole putStr (\s => ignore $ fPutStr stderr s) getChar getLine

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

||| Put a string to the console's standard output.
export
cputStr : Console => String -> RIO x ()
cputStr s = liftIO $ putStr_ %search s

||| Put a string plus trailing line break
||| to the console's standard output.
export
cputStrLn : Console => String -> RIO x ()
cputStrLn s = cputStr $ s ++ "\n"

||| Print a value to the console's standard output.
export
cprint : Console => Show a => a -> RIO x ()
cprint = cputStr . show

||| Print a value plus trailing lne break
||| to the console's standard output.
export
cprintLn : Console => Show a => a -> RIO x ()
cprintLn = cputStrLn . show

||| Put a string to the console's error output.
export
cputErr : Console => String -> RIO x ()
cputErr s = liftIO $ putErr_ %search s

||| Put a string plus trailing line break
||| to the console's error output.
export
cputErrLn : Console => String -> RIO x ()
cputErrLn s = cputErr $ s ++ "\n"

||| Print a value to the console's error output.
export
cprintErr : Console => Show a => a -> RIO x ()
cprintErr = cputErr . show

||| Print a value plus trailing lne break
||| to the console's error output.
export
cprintErrLn : Console => Show a => a -> RIO x ()
cprintErrLn = cputErrLn . show

||| Read a line from the console's standard input.
export
cgetLine : Console => RIO x String
cgetLine = liftIO $ getLine_ %search

||| Read a single character from the console's standard input.
export
cgetChar : Console => RIO x Char
cgetChar = liftIO $ getChar_ %search
