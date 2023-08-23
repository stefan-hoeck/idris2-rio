module Control.RIO.App

import public Data.List.Quantifiers.Extra
import public Control.RIO
import System

%default total

--------------------------------------------------------------------------------
--          App : A RIO with several possible error types
--------------------------------------------------------------------------------

||| A `RIO` computation, which can fail with one of several errors.
public export
0 App : (xs : List Type) -> (a : Type) -> Type
App xs a = RIO (HSum xs) a

--------------------------------------------------------------------------------
--          Throwing and injection computations that can fail
--------------------------------------------------------------------------------

||| Throw an exception to be caught in the `App` monad.
|||
||| The error type has to be in the list of accepted errors.
export
throw : Has x xs => (err : x) -> App xs a
throw err = fail (inject err)

||| Inject a `RIO` computation into one dealing with several
||| possible errors.
export
injectRIO : Has x xs => RIO x a -> App xs a
injectRIO = mapFst inject

||| Inject an `Either x a` computation into a `RIO` monad dealing
||| with several possible errors.
export
injectEither : Has x xs => Either x a -> App xs a
injectEither (Left v)  = throw v
injectEither (Right v) = pure v

||| Inject an `IO (Either x a)` computation into a `RIO` monad dealing
||| with several possible errors.
export
injectIO : Has x xs => IO (Either x a) -> App xs a
injectIO = injectRIO . liftEitherIO

--------------------------------------------------------------------------------
--          Error handling
--------------------------------------------------------------------------------

||| An error handler.
public export
0 Handler : (a,x : Type) -> Type
Handler a x = x -> RIO Void a

||| Handle a single error type in an application.
|||
||| This removes the error type from the list of possible errors
||| in the resulting `App` type.
export
handle : (prf : Has x xs) => Handler a x -> App xs a -> App (xs - x) a
handle f =
  catch $ \u =>
    case map f (decomp @{prf} u) of
      Left y  => fail y
      Right y => lift y

hall : (prf : All (Handler a) ts) => HSum ts -> RIO Void a
hall @{h :: t} (Here v)  = h v
hall @{h :: t} (There v) = hall v
hall @{[]}     x         = absurd x

||| Handle all errors, converting the computation to one that cannot fail.
export
handleAll : (prf : All (Handler a) xs) => App xs a -> RIO Void a
handleAll = catch $ \u => hall u

||| Handle all errors, converting the computation to one that cannot fail.
|||
||| This yields the provided default value in case of an error.
export
handleAllDflt : (prf : All (Handler ()) xs) => a -> App xs a -> RIO Void a
handleAllDflt v = handleAll @{mapProperty (($> v) .) prf}

||| Run an application handling all errors. This can be invoked
||| directly from  an applcation's `main` function. It invokes
||| `exitFailure` in case of an error.
export
runApp : All (Handler ()) xs -> App xs () -> IO ()
runApp hs app = do
  True <- run $ catch (\u => hall @{hs} u $> False) (app $> True)
    | False => exitFailure
  exitSuccess
