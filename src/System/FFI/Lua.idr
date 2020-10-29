module System.FFI.Lua

%default total

export
data OpaqueDict : Type where [external]

-- TODO: How we properly manage nil?
-- TODO: Functions to construct arbitrary dicts or lists without using FFI in
--       user code

public export
data Dict : List (String, Type) -> Type where
  MkDict : OpaqueDict -> Dict ts

public export
data List : List Type -> Type where
  MkList : OpaqueDict -> List ts

namespace Dict
  public export
  data FieldType : String -> Type -> List (String, Type) -> Type where
    First : FieldType n t ((n, t) :: ts)
    Later : FieldType n t ts -> FieldType n t (f :: ts)

  %foreign "function(_, _, d, n, _) return d[n] end"
  prim__getField : Dict ts -> (n : String) -> FieldType n ty ts -> ty

  %foreign "function(_, _, d, n, _, v) d[n] = v end"
  prim__setField : Dict ts -> (n : String) -> FieldType n ty ts -> ty -> PrimIO ()

  public export %inline
  getField : Dict ts -> (n : String) -> FieldType n ty ts => ty
  getField dict field @{type} = prim__getField dict field type

  public export %inline
  setField : HasIO io => Dict ts -> (n : String) -> FieldType n ty ts => ty -> io ()
  setField dict field val @{_} @{type} = primIO $ prim__setField dict field type val

namespace List
  public export
  data FieldType : Nat -> Type -> List Type -> Type where
    First : FieldType 1 t (t :: ts)
    Later : FieldType n t ts -> FieldType (S n) t (t' :: ts)

  %foreign "function(_, _, l, n, _) return l[n] end"
  prim__getField : List ts -> (n : Nat) -> FieldType n ty ts -> ty

  %foreign "function(_, _, l, n, _, v) d[n] = v end"
  prim__setField : List ts -> (n : Nat) -> FieldType n ty ts -> ty -> PrimIO ()

  public export %inline
  getField : List ts -> (n : Nat) -> FieldType n ty ts => ty
  getField list field @{type} = prim__getField list field type

  public export %inline
  setField : HasIO io => List ts -> (n : Nat) -> FieldType n ty ts => ty -> io ()
  setField list field val @{_} @{type} = primIO $ prim__setField list field type val

%foreign "function(v) return v == nil end"
prim__isNil : OpaqueDict -> PrimIO Bool

isNil : HasIO io => OpaqueDict -> io Bool
isNil = primIO . prim__isNil
