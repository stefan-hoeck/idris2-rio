module Control.RIO.Console

import Data.IORef
import System.File

%default total

--------------------------------------------------------------------------------
--          ConsoleOut
--------------------------------------------------------------------------------

||| Record representing a console with
||| standard output and error output
public export
record ConsoleOut where
  constructor MkConsoleOut
  putStr_   : String -> IO ()
  putErr_   : String -> IO ()

||| The default console, printing to standard out and standard err.
export
stdOut : ConsoleOut
stdOut = MkConsoleOut putStr (\s => ignore $ fPutStr stderr s)

||| Put a string to the console's standard output.
export %inline
cputStr : ConsoleOut => HasIO io => String -> io ()
cputStr s = liftIO $ putStr_ %search s

||| Put a string plus trailing line break
||| to the console's standard output.
export %inline
cputStrLn : ConsoleOut => HasIO io => String -> io ()
cputStrLn s = cputStr $ s ++ "\n"

||| Print a value to the console's standard output.
export %inline
cprint : ConsoleOut => Show a => HasIO io => a -> io ()
cprint = cputStr . show

||| Print a value plus trailing lne break
||| to the console's standard output.
export
cprintLn : ConsoleOut => Show a => HasIO io => a -> io ()
cprintLn = cputStrLn . show

||| Put a string to the console's error output.
export
cputErr : ConsoleOut => HasIO io => String -> io ()
cputErr s = liftIO $ putErr_ %search s

||| Put a string plus trailing line break
||| to the console's error output.
export
cputErrLn : ConsoleOut => HasIO io => String -> io ()
cputErrLn s = cputErr $ s ++ "\n"

||| Print a value to the console's error output.
export
cprintErr : ConsoleOut => Show a => HasIO io => a -> io ()
cprintErr = cputErr . show

||| Print a value plus trailing lne break
||| to the console's error output.
export
cprintErrLn : ConsoleOut => Show a => HasIO io => a -> io ()
cprintErrLn = cputErrLn . show

--------------------------------------------------------------------------------
--          ConsoleIn
--------------------------------------------------------------------------------

||| Record representing a console with standard input.
public export
record ConsoleIn where
  constructor MkConsoleIn
  getChar_  : IO Char
  getLine_  : IO String

||| The default console, reading from standard input.
export
stdIn : ConsoleIn
stdIn = MkConsoleIn getChar getLine

||| Read a line from the console's standard input.
export
cgetLine : ConsoleIn => HasIO io => io String
cgetLine = liftIO $ getLine_ %search

||| Read a single character from the console's standard input.
export
cgetChar : ConsoleIn => HasIO io => io Char
cgetChar = liftIO $ getChar_ %search

--------------------------------------------------------------------------------
--          Console
--------------------------------------------------------------------------------

public export
record Console where
  constructor MkConsole
  cin : ConsoleIn
  out : ConsoleOut

export
stdIO : Console
stdIO = MkConsole stdIn stdOut

export %hint %inline
consoleOut : Console -> ConsoleOut
consoleOut = out

export %hint %inline
consoleIn : Console -> ConsoleIn
consoleIn = cin
