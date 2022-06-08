module Control.RIO

%default total

||| A stack-safe, effectful computation running with
||| environment `e`, producing a value of type `a`, with
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
||| automatically in `IO` itself.
public export
data RIO : (e : Type) -> (x : Type) -> (a : Type) -> Type where
  Lift  : (run : e -> PrimIO (Either x a)) -> RIO e x a
  Chain : RIO e x1 a -> (Either x1 a -> RIO e x2 b) -> RIO e x2 b

-- Computation stack for evaluating `RIO` actions.
data Stack : (e,x1,x2,a,b : Type) -> Type where
  Nil  :  Stack e x x a a
  (::) :  (Either x1 a -> RIO e y v)
       -> Stack e y  x2 v b
       -> Stack e x1 x2 a b

||| Tail-recursively evaluate a `RIO` computation.
export
eval : RIO e x a -> e -> IO (Either x a)
eval app vx = fromPrim $ go app []
  where go :  RIO e x1 v
           -> Stack e x1 x2 v w
           -> PrimIO (Either x2 w)
        go (Chain z f) st       w = go z (f :: st) w
        go (Lift run) []        w = run vx w
        go (Lift run) (f :: fs) w =
          let MkIORes ei w2 = run vx w
           in assert_total $ go (f ei) fs w2

||| Return the current environment.
export
ask : RIO e x e
ask = Lift $ \v,w => MkIORes (Right v) w

||| Map a function over the returned environment.
export
asks : (e -> f) -> RIO e x f
asks f = Lift $ \v,w => MkIORes (Right $ f v) w

||| Wrap an `Either x a` in a `RIO` computation.
export
liftEither : Either x a -> RIO e x a
liftEither v = Lift $ \_,w => MkIORes v w

||| Map a function over the result over a `RIO` computation.
export
mapApp : (Either x a -> Either y b) -> RIO e x a -> RIO e y b
mapApp f app = Chain app (liftEither . f)

||| Fail with an error.
export
throw : x -> RIO e x a
throw err = liftEither (Left err)

bindApp : RIO e x a -> (a -> RIO e x b) -> RIO e x b
bindApp app f = Chain app (either throw f)

export %inline
Functor (RIO e x) where
  map = mapApp . map

export %inline
Applicative (RIO e x) where
  pure v    = liftEither (Right v)
  af <*> aa = bindApp af (\f => map f aa)

export %inline
Monad (RIO e x) where
  (>>=) = bindApp

export
HasIO (RIO e x) where
  liftIO io = Lift $ \_ => toPrim (map Right io)

||| Catch an exception with the given handler.
export
catch : (x -> RIO e y a) -> RIO e x a -> RIO e y a
catch f app = Chain app (either f pure)

||| Makes sure the given (cleanup) action is run at the end
||| of a computation even in case of an error.
export
finally : RIO e x () -> RIO e x a -> RIO e x a
finally cleanup app =
  Chain app (either (\e => cleanup >> throw e) (\v => cleanup $> v))
