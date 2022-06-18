module Control.RIO.App

import Data.List.Quantifiers
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

export 
handle : (prf : Has x xs) => (x -> RIO Void a) -> App xs a -> App (xs - x) a
handle f = catch $ \u => case handle f u of
  Left y  => fail y
  Right y => lift y

public export
0 Handler : (a,x : Type) -> Type
Handler a x = x -> RIO Void a

export 
handleAll : (prf : All (Handler a) xs) => App xs a -> RIO Void a
handleAll = catch $ \u => Union.handleAll u
