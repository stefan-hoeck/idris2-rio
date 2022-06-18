module Control.RIO

%default total

||| A stack-safe, effectful computation
||| producing a value of type `a`, with
||| the possibility of an error of type `x`.
|||
||| We get flexible error handling by using an open union
||| over a list of types for `x` (see `Control.RIO.App`),
||| and we get flexible, mockable effects by providing the
||| corresponding implementations (typically mere records)
||| via the envirionment.
|||
||| About the name: This stems originally from Haskell, where
||| `RIO` is an abbreviation for `ReaderT IO`. In Idris, we need
||| to add exceptions to the stack, which will not be handled
||| automatically in `IO` itself. On the other hand, we can just
||| use implicit arguments for our effects, so there is no
||| need for an envirionment in the monad itself.
public export
data RIO : (x : Type) -> (a : Type) -> Type where
  Lift  : (run : PrimIO (Either x a)) -> RIO x a
  Chain : RIO x1 a -> (Either x1 a -> RIO x2 b) -> RIO x2 b

-- Computation stack for evaluating `RIO` actions.
data Stack : (x1,x2,a,b : Type) -> Type where
  Nil  : Stack x x a a
  (::) : (Either x1 a -> RIO y v) -> Stack y  x2 v b -> Stack x1 x2 a b

||| Tail-recursively evaluate a `RIO` computation.
export
eval : RIO x a -> IO (Either x a)
eval app = fromPrim $ go app []
  where go : RIO x1 v -> Stack x1 x2 v w -> PrimIO (Either x2 w)
        go (Chain z f) st       w = go z (f :: st) w
        go (Lift run) []        w = run w
        go (Lift run) (f :: fs) w =
          let MkIORes ei w2 = run w
           in assert_total $ go (f ei) fs w2

||| Tail-recursively evaluate a `RIO` computation,
||| which cannot fail with an exception.
export
run : Uninhabited x => RIO x a -> IO a
run app = either absurd id <$> eval app

||| Wrap an `Either x a` in a `RIO` computation.
export
liftEither : Either x a -> RIO x a
liftEither v = Lift $ \w => MkIORes v w

||| Wrap an `IO (Either x a)` in a `RIO` computation.
export
liftEitherIO : IO (Either x a) -> RIO x a
liftEitherIO io = Lift $ toPrim io

||| Map a function over the result over a `RIO` computation.
export
mapApp : (Either x a -> Either y b) -> RIO x a -> RIO y b
mapApp f app = Chain app (liftEither . f)

||| Fail with an error.
export
fail : x -> RIO x a
fail err = liftEither (Left err)

bindApp : RIO x a -> (a -> RIO x b) -> RIO x b
bindApp app f = Chain app (either fail f)

export %inline
Functor (RIO x) where
  map = mapApp . map

export %inline
Bifunctor RIO where
  bimap f g = mapApp (bimap f g)

export %inline
Applicative (RIO x) where
  pure v    = liftEither (Right v)
  af <*> aa = bindApp af (\f => map f aa)

export %inline
Monad (RIO x) where
  (>>=) = bindApp

export
HasIO (RIO x) where
  liftIO io = Lift $ toPrim (map Right io)

||| Catch an exception with the given handler.
export
catch : (x -> RIO y a) -> RIO x a -> RIO y a
catch f app = Chain app (either f pure)

||| Makes sure the given (cleanup) action is run at the end
||| of a computation even in case of an error.
export
finally : RIO x () -> RIO x a -> RIO x a
finally cleanup app =
  Chain app (either (\e => cleanup >> fail e) (\v => cleanup $> v))

export 
lift : Uninhabited x => RIO x a -> RIO y a
lift io = catch (\v => absurd v) io
