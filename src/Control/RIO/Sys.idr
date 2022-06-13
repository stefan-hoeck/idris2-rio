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

||| Record representing capabilities for running system calls
public export
record Sys_ where
  constructor MkSys
  sys_ : String -> IO (Either SysErr ())
  run_ : String -> IO (Either SysErr String)

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

public export
interface Sys (0 e : Type) where
  sys_ : e -> Sys_

export
sys : Sys e => Has SysErr xs => String -> App e xs ()
sys cmd = asks sys_ >>= \s => injectIO (s.sys_ cmd)

export
run : Sys e => Has SysErr xs => String -> App e xs String
run cmd = asks sys_ >>= \s => injectIO (s.run_ cmd)

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
system : Sys_
system = MkSys sysImpl runImpl

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

||| Forcefully deletes a directory with all its content
export
rmDir : Sys e => FS e => Has SysErr xs => FilePath -> App e xs ()
rmDir dir = when !(exists dir) $ sys "rm -rf \{dir}"
