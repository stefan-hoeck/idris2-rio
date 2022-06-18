module Control.RIO.State

import Data.IORef
import Control.RIO

%default total

||| Mutable state holding values of type `a` and tagged
||| with label `lbl`.
public export
data State : (lbl : l) -> (a : Type) -> Type where
  [search lbl]
  ST : IORef a -> State lbl a

readST : State lbl a -> RIO x a
readST (ST ref) = liftIO $ readIORef ref

writeST : (v : a) -> State lbl a -> RIO x ()
writeST v (ST ref) = liftIO $ writeIORef ref v

modifyST : (v : a -> a) -> State lbl a -> RIO x ()
modifyST f (ST ref) = liftIO $ modifyIORef ref f

||| Read the current value of a mutable reference.
export %inline
getAt : (0 lbl : l) -> (st : State lbl a) => RIO x a
getAt _ = readST st

||| Write a value to a mutable reference.
export %inline
setAt : (0 lbl : l) -> (st : State lbl a) => a -> RIO x ()
setAt _ v = writeST v st

||| Modify the value stored in a mutable reference.
export %inline
modifyAt : (0 lbl : l) -> (st : State lbl a) => (a -> a) -> RIO x ()
modifyAt _ f = modifyST f st
