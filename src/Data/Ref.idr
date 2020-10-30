module Data.Ref

import Data.IORef

%default total

export
data Ref : (l : label) -> Type -> Type where
  [search l]
  MkRef : IORef a -> Ref l a

export
newRef : HasIO io => (l : label) -> t -> io (Ref l t)
newRef l val = do ref <- newIORef val
                  pure (MkRef ref)

export %inline
get : HasIO io => (l : label) -> Ref l t => io t
get @{_} l @{MkRef ref} = readIORef ref

export %inline
put : HasIO io => (l : label) -> Ref l t => t -> io ()
put @{_} l @{MkRef ref} val = writeIORef ref val

export %inline
modify : HasIO io => (l : label) -> Ref l t => (t -> t) -> io ()
modify @{_} l @{MkRef ref} f = readIORef ref >>= writeIORef ref . f
