module Data.Union

import Data.List.Quantifiers

%default total

||| Proof that a value is present in a list. This is
||| isomorphic to `Data.List.Elem` but with (in my opinion)
||| more fitting names for our use case.
public export
data Has : (v : a) -> (ts : List a) -> Type where
  Z : Has v (v :: vs)
  S : Has v vs -> Has v (w :: vs)

Uninhabited (Has v []) where
  uninhabited Z impossible
  uninhabited (S _) impossible

||| Removes an element from a list. This is used to
||| calculate the list of effects after a single effect
||| was properly handled.
public export
0 (-) : (ts : List a) -> (v : a) -> (prf : Has v ts) => List a
(-) (_ :: vs)      _ {prf = Z}   = vs
(-) (y :: x :: xs) v {prf = S k} = y :: (-) (x :: xs) v

||| Sum type holding a value of type `t`, which must be
||| on of the types listed in `ts`.
public export
data Union : (ts : List Type) -> Type where
  U : (ix : Has t ts) -> (val : t) -> Union ts

public export
Uninhabited (Union []) where
  uninhabited (U ix v) = absurd ix

||| Inject a value into a `Union`.
public export %inline
inj : (prf : Has t ts) => t -> Union ts
inj = U prf

||| Tries to extract a value from a `Union`.
public export
prj : (prf : Has t ts) => Union ts -> Maybe t
prj {prf = Z}    (U Z v)     = Just v
prj {prf = S ix} (U (S x) v) = prj {prf = ix} (U x v)
prj _                        = Nothing

||| Extracts the last value from a unary sum.
public export
prj1 : Union [t] -> t
prj1 (U Z val) = val
prj1 (U (S x) val) impossible

||| Prepend a new type to an existing `Union` value.
export
weaken : Union ts -> Union (t :: ts)
weaken (U ix val) = U (S ix) val

||| Extract one of the values from a `Union`.
public export
decomp : (prf : Has t ts) => Union ts -> Either (Union (ts - t)) t
decomp {prf = Z}                      (U Z     val) = Right $ val
decomp {prf = Z}                      (U (S x) val) = Left $ U x val
decomp {prf = S y} {ts = f :: h :: t} (U Z val)     = Left $ U Z val
decomp {prf = S y} {ts = f :: h :: t} (U (S x) val) =
  mapFst weaken $ decomp (U x val)

||| Handle one of the values in a `Union`.
public export
handle : (prf : Has t ts) => (t -> r) -> Union ts -> Either (Union (ts - t)) r
handle g = map g . decomp

||| Handle all of the values in a `Union`.
public export
handleAll : (prf : All (\t => t -> a) ts) => Union ts -> a
handleAll {prf = h :: t} (U Z val)     = h val
handleAll {prf = h :: t} (U (S x) val) = handleAll (U x val)
handleAll {prf = []}     x             = absurd x
