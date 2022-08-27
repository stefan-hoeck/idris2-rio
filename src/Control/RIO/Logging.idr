module Control.RIO.Logging

import Control.ANSI
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

export
Interpolation LogLevel where
  interpolate Trace   = "trace"
  interpolate Debug   = "debug"
  interpolate Info    = "info"
  interpolate Warning = "warning"
  interpolate Error   = "error"

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

export
consoleLogger : ConsoleOut -> (LogLevel -> Lazy String -> String) -> Logger
consoleLogger c f = MkLogger $ \l,s => case l of
  Error => c.putErr_ (f l s ++ "\n")
  _     => c.putStr_ (f l s ++ "\n")

export
basicConsoleLogger : ConsoleOut -> Logger
basicConsoleLogger c = consoleLogger c $ \l,s => "[\{l}] \{s}"

col : LogLevel -> String
col Trace   = show $ colored White "trace"
col Debug   = show $ colored Cyan "debug"
col Info    = show $ colored Green "info"
col Warning = show $ colored Yellow "warning"
col Error   = show $ colored Red "error"

||| A console logger with colored log level tags
export
colorConsoleLogger : ConsoleOut -> Logger
colorConsoleLogger c = consoleLogger c $ \l,s => "[\{col l}] \{s}"

--------------------------------------------------------------------------------
--          Syslog
--------------------------------------------------------------------------------

public export
data Facility =
    Kernel
  | UserLevel
  | MailSystem
  | SystemDaemon
  | Authorization
  | SyslogInternal
  | LinePrinter
  | NetworkNews
  | UUCP
  | ClockDaemon
  | SecurityMessages
  | FTPDaemon
  | NTP
  | LogAudit
  | LogAlert
  | ClockDaemon2

facility : Facility -> Nat
facility Kernel           = 0
facility UserLevel        = 1
facility MailSystem       = 2
facility SystemDaemon     = 3
facility Authorization    = 4
facility SyslogInternal   = 5
facility LinePrinter      = 6
facility NetworkNews      = 7
facility UUCP             = 8
facility ClockDaemon      = 9
facility SecurityMessages = 10
facility FTPDaemon        = 11
facility NTP              = 12
facility LogAudit         = 13
facility LogAlert         = 14
facility ClockDaemon2     = 15

severity : LogLevel -> Nat
severity Trace   = 7
severity Debug   = 7
severity Info    = 6
severity Warning = 5
severity Error   = 3

||| A logger using syslog priority codes. This can be used with
||| systemd services.
export
syslogLogger : Facility -> ConsoleOut -> Logger
syslogLogger f c =
  consoleLogger c $
    \l,s =>
      let lvl = 8 * facility f + severity l
       in "<\{show lvl}> \{s}"

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
