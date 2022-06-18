module Control.RIO.Logging

import Control.RIO.App
import Control.RIO.Console

%default total

--------------------------------------------------------------------------------
--          Log Level
--------------------------------------------------------------------------------

public export
data LogLevel = Trace | Debug | Info | Warning | Error

public export
priority : LogLevel -> Nat
priority Trace   = 0
priority Debug   = 1
priority Info    = 2
priority Warning = 3
priority Error   = 4

public export
Eq LogLevel where (==) = (==) `on` priority

public export
Ord LogLevel where compare = compare `on` priority

--------------------------------------------------------------------------------
--          Record
--------------------------------------------------------------------------------

public export
record Logger where
  constructor MkLogger
  log : LogLevel -> Lazy String -> IO ()

||| Only log message of at least the given logging level.
export
filter : LogLevel -> Logger -> Logger
filter lvl x = MkLogger $ \l,s => case l >= lvl of
  True  => x.log l s
  False => pure ()

export
Semigroup Logger where
  x <+> y = MkLogger $ \l,s => x.log l s >> y.log l s

export
Monoid Logger where
  neutral = MkLogger $ \_,_ => pure ()

-- export
-- consoleLogger : Console -> Logger
-- consoleLogger c = MkLogger $ \l,s => ?foo

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

export
log : Logger => LogLevel -> Lazy String -> RIO x ()
log l s = liftIO (log %search l s)

export %inline
trace : Logger => Lazy String -> RIO x ()
trace = log Trace

export %inline
debug : Logger => Lazy String -> RIO x ()
debug = log Debug

export %inline
info : Logger => Lazy String -> RIO x ()
info = log Info

export %inline
warn : Logger => Lazy String -> RIO x ()
warn = log Warning

export %inline
error : Logger => Lazy String -> RIO x ()
error = log Error
