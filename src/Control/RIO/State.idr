module Control.RIO.State

import Data.IORef
import Control.RIO

%default total

||| Mutable state holding values of type `a` and tagged
||| with label `lbl`.
public export
data State_ : (lbl : l) -> (a : Type) -> Type where
  [search lbl]
  ST : IORef a -> State_ lbl a

readST : State_ lbl a -> RIO e x a
readST (ST ref) = liftIO $ readIORef ref

writeST : (v : a) -> State_ lbl a -> RIO x e ()
writeST v (ST ref) = liftIO $ writeIORef ref v

modifyST : (v : a -> a) -> State_ lbl a -> RIO x e ()
modifyST f (ST ref) = liftIO $ modifyIORef ref f

||| Witness that environment `e` holds a mutable reference
||| holding values of type `a` and tagged with label `lbl`.
|||
||| The label `lbl` is used to distinguish between different
||| states used at the same time.
public export
interface State (0 e : Type) (0 lbl : l) (0 a : Type) | e,lbl where
  getState_ : e -> State_ lbl a 

export
State (State_ lbl a) lbl a where
  getState_ v = v

||| Extract a mutable reference from the environment
export
state : (0 lbl : l) -> State e lbl a => RIO e x (State_ lbl a)
state lbl = asks getState_

||| Read the current value of a mutable reference.
export
getAt : (0 lbl : l) -> State e lbl a => RIO e x a
getAt lbl = state lbl >>= readST

||| Write a value to a mutable reference.
export
setAt : (0 lbl : l) -> State e lbl a => a -> RIO e x ()
setAt lbl v = state lbl >>= writeST v

||| Modify the value stored in a mutable reference.
export
modifyAt : (0 lbl : l) -> State e lbl a => (a -> a) -> RIO e x ()
modifyAt lbl f = state lbl >>= modifyST f
