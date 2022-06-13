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
record Logger_ where
  constructor MkLogger
  log : LogLevel -> Lazy String -> IO ()

||| Only log message of at least the given logging level.
export
filter : LogLevel -> Logger_ -> Logger_
filter lvl x = MkLogger $ \l,s => case l >= lvl of
  True  => x.log l s
  False => pure ()

export
Semigroup Logger_ where
  x <+> y = MkLogger $ \l,s => x.log l s >> y.log l s

export
Monoid Logger_ where
  neutral = MkLogger $ \_,_ => pure ()

export
consoleLogger : Console_ -> Logger_
consoleLogger c = MkLogger $ \l,s => ?foo

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

public export
interface Logger (0 e : Type) where
  logger_ : e -> Logger_

export
logger : Logger e => RIO e x Logger_
logger = asks logger_

export
log : Logger e => LogLevel -> Lazy String -> RIO e x ()
log l s = logger >>= \x => liftIO (x.log l s)

export %inline
trace : Logger e => Lazy String -> RIO e x ()
trace = log Trace

export %inline
debug : Logger e => Lazy String -> RIO e x ()
debug = log Debug

export %inline
info : Logger e => Lazy String -> RIO e x ()
info = log Info

export %inline
warn : Logger e => Lazy String -> RIO e x ()
warn = log Warning

export %inline
error : Logger e => Lazy String -> RIO e x ()
error = log Error
