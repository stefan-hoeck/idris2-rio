# idris2-rio: A simple, stack-safe effect system based on the RIO Monad

In Haskell, the [*RIO* monad](https://hackage.haskell.org/package/rio-0.1.19.0/docs/RIO.html#g:2)
is a specialized form of `ReaderT env IO`, which can be used to implement
a simple but versatile effect system, where effects are available via the
environment `env`.

In Idris, we need an additional effect layer for error handling. This library
therefore provides a simple, stack-safe effect type, `RIO e x a`, where
`e` is the environment we need, `x` is the type of error our computation
can throw, and `a` is the result type if all goes well.

Such a type can be enhanced with several effects such as mutable state
or a file system, by making the corresponding functionality available
through the environment.
