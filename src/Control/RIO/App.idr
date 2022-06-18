module Control.RIO.App

import public Data.Union
import public Control.RIO

%default total

||| A `RIO` computation, which can fail with one of several errors.
public export
0 App : (xs : List Type) -> (a : Type) -> Type
App xs a = RIO (Union xs) a

export
throw : Has x xs => (err : x) -> App xs a
throw err = fail (inj err)

export
inject : Has x xs => RIO x a -> App xs a
inject = mapFst inj

export
injectIO : Has x xs => IO (Either x a) -> App xs a
injectIO = inject . liftEitherIO
