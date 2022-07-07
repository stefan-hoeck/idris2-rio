module Control.RIO.Sys

import Control.RIO.App
import Control.RIO.File
import System

%default total

--------------------------------------------------------------------------------
--          Record
--------------------------------------------------------------------------------

public export
record SysErr where
  constructor MkSE
  cmd : String
  err : Int

export
printErr : SysErr -> String
printErr (MkSE cmd err) =
  "Command terminated with error code \{show err}: \{cmd}"

||| Record representing capabilities for running system calls
public export
record Sys where
  constructor MkSys
  sys_ : String -> IO (Either SysErr ())
  run_ : String -> IO (Either SysErr String)

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

export
sys : Sys => Has SysErr xs => String -> App xs ()
sys cmd = injectIO (sys_ %search cmd)

export
run : Sys => Has SysErr xs => String -> App xs String
run cmd = injectIO (run_ %search cmd)

--------------------------------------------------------------------------------
--          Implementation
--------------------------------------------------------------------------------

sysImpl : String -> IO (Either SysErr ())
sysImpl cmd = do
  0 <- system cmd | n => pure (Left $ MkSE cmd n)
  pure (Right ())

covering
runImpl : String -> IO (Either SysErr String)
runImpl cmd = do
  (res,0) <- System.run cmd | (_,n) => pure (Left $ MkSE cmd n)
  pure (Right res)

export covering
system : Sys
system = MkSys sysImpl runImpl

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

||| Forcefully deletes a directory with all its content
export
rmDir : Sys => FS => Has SysErr xs => Path t -> App xs ()
rmDir dir = when !(exists dir) $ sys "rm -rf \{dir}"
