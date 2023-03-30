module Control.RIO.State

import Data.IORef

%default total

--------------------------------------------------------------------------------
--          Getter
--------------------------------------------------------------------------------

||| Effectful getter, yielding values of type `a` and tagged
||| with label `lbl`.
public export
data Get : (lbl : l) -> (a : Type) -> Type where
  [search lbl]
  MkGet :  (get_  : IO a) -> Get lbl a

||| Read the current value of a getter.
export %inline
getAt : (0 lbl : l) -> {auto get : Get lbl a} -> HasIO io => io a
getAt _ @{MkGet g} = liftIO g

--------------------------------------------------------------------------------
--          Setter
--------------------------------------------------------------------------------

||| Effectful setter, allowing us to write a mutable value
||| with label `lbl`.
public export
data Set : (lbl : l) -> (a : Type) -> Type where
  [search lbl]
  MkSet :  (set_ : a -> IO ()) -> Set lbl a

||| Write a value to a mutable reference.
export %inline
setAt : (0 lbl : l) -> {auto set : Set lbl a} -> HasIO io => a -> io ()
setAt _ @{MkSet w} v = liftIO (w v)

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

export %hint %inline
stToGet : ST lbl a => Get lbl a
stToGet @{MkST r _ _} = MkGet r

export %hint %inline
stToSet : ST lbl a => Set lbl a
stToSet @{MkST _ s _} = MkSet s

||| Viewing a mutable state through a getter and a setter.
export
mapST : (a -> b) -> (b -> a -> a) -> ST lbl a -> ST lbl2 b
mapST f g (MkST r w m) =
  MkST (map f r) (\v => m (g v)) (\h => m (\va => g (h $ f va) va))

||| Modify the value stored in a mutable reference.
export %inline
modifyAt : (0 lbl : l) -> {auto st : ST lbl a} -> HasIO io => (a -> a) -> io ()
modifyAt _ @{MkST _ _ m} f = liftIO (m f)

--------------------------------------------------------------------------------
--          IORef Impl
--------------------------------------------------------------------------------

||| Mutable state holding values of type `a` and tagged
||| with label `lbl`.
public export
data State : (lbl : l) -> (a : Type) -> Type where
  [search lbl]
  MkState : IORef a -> State lbl a

export %inline %hint
stateToST : (s : State lbl a) => ST lbl a
stateToST {s = MkState r} = MkST (readIORef r) (writeIORef r) (modifyIORef r)
