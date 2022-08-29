module Control.RIO.State

import Data.IORef
import Control.RIO

%default total

--------------------------------------------------------------------------------
--          State
--------------------------------------------------------------------------------

||| Mutable state holding values of type `a` and tagged
||| with label `lbl`.
public export
data ST : (lbl : l) -> (a : Type) -> Type where
  [search lbl]
  MkST :  (read_  : IO a)
       -> (write_ : a -> IO ())
       -> (mod_   : (a -> a) -> IO ())
       -> ST lbl a

||| Viewing a mutable state through a getter and a setter.
export
mapST : (a -> b) -> (b -> a -> a) -> ST lbl a -> ST lbl2 b
mapST f g (MkST r w m) =
  MkST (map f r) (\v => m (g v)) (\h => m (\va => g (h $ f va) va))

||| Read the current value of a mutable reference.
export %inline
getAt : (0 lbl : l) -> (st : ST lbl a) => RIO x a
getAt _ {st = MkST r _ _} = liftIO r

||| Write a value to a mutable reference.
export %inline
setAt : (0 lbl : l) -> (st : ST lbl a) => a -> RIO x ()
setAt _ {st = MkST _ w _} v = liftIO (w v)

||| Modify the value stored in a mutable reference.
export %inline
modifyAt : (0 lbl : l) -> (st : ST lbl a) => (a -> a) -> RIO x ()
modifyAt _ {st = MkST _ _ m} f = liftIO (m f)

--------------------------------------------------------------------------------
--          IORef Impl
--------------------------------------------------------------------------------

||| Mutable state holding values of type `a` and tagged
||| with label `lbl`.
public export
data State : (lbl : l) -> (a : Type) -> Type where
  [search lbl]
  MkState : IORef a -> State lbl a

export %inline
stateToST : (s : State lbl a) => ST lbl a
stateToST {s = MkState r} = MkST (readIORef r) (writeIORef r) (modifyIORef r)
