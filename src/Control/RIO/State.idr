module Control.RIO.State

import Data.IORef

%default total

--------------------------------------------------------------------------------
--          Reader
--------------------------------------------------------------------------------

||| Effectful getter, yielding values of type `a` and tagged
||| with label `lbl`.
public export
data Read : (lbl : l) -> (a : Type) -> Type where
  [search lbl]
  MkRead :  (read_  : IO a) -> Read lbl a

||| Read the current value of a getter.
export %inline
readAt : (0 lbl : l) -> {auto r : Read lbl a} -> HasIO io => io a
readAt _ @{MkRead g} = liftIO g

namespace Read

  ||| Read the current value of a getter.
  |||
  ||| Use this if you experience slowdowns with `readAt` durcing compilation.
  export %inline
  (.read) : HasIO io => Read lbl a -> io a
  (.read) (MkRead r) = liftIO r

--------------------------------------------------------------------------------
--          State
--------------------------------------------------------------------------------

||| Mutable state holding values of type `a` and tagged
||| with label `lbl`.
public export
data ST : (lbl : l) -> (a : Type) -> Type where
  [search lbl]
  MkST :
       (read_  : IO a)
    -> (write_ : a -> IO ())
    -> (mod_   : (a -> a) -> IO ())
    -> ST lbl a

export
mkST : HasIO io => a -> io (ST lbl a)
mkST v = do
  ref <- newIORef v
  pure $ MkST (readIORef ref) (writeIORef ref) (modifyIORef ref)

export %hint %inline
stToRead : ST lbl a => Read lbl a
stToRead @{MkST r _ _} = MkRead r

||| Viewing a mutable state through a getter and a setter.
export
mapST : (a -> b) -> (b -> a -> a) -> ST lbl a -> ST lbl2 b
mapST f g (MkST r w m) =
  MkST (map f r) (\v => m (g v)) (\h => m (\va => g (h $ f va) va))

||| Modify the value stored in a mutable reference.
export %inline
modifyAt : (0 lbl : l) -> {auto st : ST lbl a} -> HasIO io => (a -> a) -> io ()
modifyAt _ @{MkST _ _ m} f = liftIO (m f)

||| Write a value to a mutable reference.
export %inline
getAt : (0 lbl : l) -> {auto set : ST lbl a} -> HasIO io => io a
getAt _ @{MkST r _ _} = liftIO r

||| Write a value to a mutable reference.
export %inline
setAt : (0 lbl : l) -> {auto set : ST lbl a} -> HasIO io => a -> io ()
setAt _ @{MkST _ w _} v = liftIO (w v)

||| Read the current value of some mutable state.
|||
||| Use this if you experience slowdowns with `getAt` durcing compilation.
export %inline
(.get) : HasIO io => ST lbl a -> io a
(.get) (MkST r _ _) = liftIO r

||| Overwrite the current value of some mutable state.
|||
||| Use this if you experience slowdowns with `setAt` durcing compilation.
export %inline
(.set) : HasIO io => ST lbl a -> a -> io ()
(.set) (MkST _ w _) v = liftIO $ w v

||| Modify the current value of some mutable state.
|||
||| Use this if you experience slowdowns with `setAt` durcing compilation.
export %inline
(.mod) : HasIO io => ST lbl a -> (a -> a) -> io ()
(.mod) (MkST _ _ m) f = liftIO $ m f
