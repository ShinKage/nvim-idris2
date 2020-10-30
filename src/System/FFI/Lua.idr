module System.FFI.Lua

import Data.List

%default total

export
data OpaqueDict : Type where [external]

-- TODO: How we properly manage nil?

public export
data Dict : List (String, Type) -> Type where
  MkDict : OpaqueDict -> Dict ts

public export
data List : List Type -> Type where
  MkList : (n : Nat) -> OpaqueDict -> n = length ts => List ts

%foreign "function() return {} end"
prim__empty : OpaqueDict

namespace Dict
  public export
  data FieldType : String -> Type -> List (String, Type) -> Type where
    First : FieldType n t ((n, t) :: ts)
    Later : FieldType n t ts -> FieldType n t (f :: ts)

  %foreign "function(_, d, n) return d[n] end"
  prim__getField : OpaqueDict -> (n : String) -> ty

  %foreign "function(_, d, n, v) d[n] = v end"
  prim__setField : OpaqueDict -> (n : String) -> ty -> PrimIO ()

  public export %inline
  getField : Dict ts -> (n : String) -> FieldType n ty ts => ty
  getField (MkDict dict) field = prim__getField dict field

  public export %inline
  setField : HasIO io => Dict ts -> (n : String) -> FieldType n ty ts => ty -> io ()
  setField (MkDict dict) field val = primIO $ prim__setField dict field val

  export
  empty : Dict []
  empty = MkDict prim__empty

  export
  addField : HasIO io => (n : String) -> (val : t) -> Dict ts -> io (Dict ((n, t) :: ts))
  addField n val (MkDict dict) = do
    primIO $ prim__setField dict n val
    pure (MkDict dict)

namespace List
  public export
  data FieldType : Nat -> Type -> List Type -> Type where
    First : FieldType 1 t (t :: ts)
    Later : FieldType n t ts -> FieldType (S n) t (t' :: ts)

  %foreign "function(_, l, n) return l[n] end"
  prim__getField : OpaqueDict -> (n : Nat) -> ty

  %foreign "function(_, l, n, v) d[n] = v end"
  prim__setField : OpaqueDict -> (n : Nat) -> ty -> PrimIO ()

  public export %inline
  getField : List ts -> (n : Nat) -> FieldType n ty ts => ty
  getField (MkList _ list) field = prim__getField list field

  public export %inline
  setField : HasIO io => List ts -> (n : Nat) -> FieldType n ty ts => ty -> io ()
  setField (MkList _ list) field val = primIO $ prim__setField list field val

  export
  empty : List []
  empty = MkList _ prim__empty

  lengthSuc : (xs : List a) -> (y : a) -> (ys : List a) ->
              length (xs ++ (y :: ys)) = S (length (xs ++ ys))
  lengthSuc [] _ _ = Refl
  lengthSuc (x :: xs) y ys = cong S (lengthSuc xs y ys)

  export
  addField : HasIO io => (val : t) -> List ts -> io (List (ts ++ [t]))
  addField val (MkList size list @{prf}) = do
    primIO $ prim__setField list (S size) val
    pure (MkList (S size) list @{rewrite prf in
                                 rewrite lengthSuc ts t [] in
                                 rewrite appendNilRightNeutral ts in Refl})

%foreign "function(v) return v == nil end"
prim__isNil : OpaqueDict -> PrimIO Bool

isNil : HasIO io => OpaqueDict -> io Bool
isNil = primIO . prim__isNil
