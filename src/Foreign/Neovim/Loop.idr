module Foreign.Neovim.Loop

import public System.FFI.Lua

%default total

export
data Stream : Type where
  MkStream : OpaqueDict -> Stream

%foreign "function(_) return vim.loop.new_pipe(false) end"
prim__newPipe : PrimIO OpaqueDict

export
newPipe : HasIO io => io Stream
newPipe = MkStream <$> primIO prim__newPipe

export
data Socket : Type where
  MkSocket : OpaqueDict -> Socket

%foreign "function(_) return vim.loop.new_tcp() end"
prim__newTCP : PrimIO OpaqueDict

export
newTCP : HasIO io => io Socket
newTCP = MkSocket <$> primIO prim__newTCP

prim__spawn__ffi : String
prim__spawn__ffi =
    "function(cmd)"
 ++ "    return function(opts)"
 ++ "        return function (callback)"
 ++ "           return function (_)"
 ++ "               return vim.loop.spawn(cmd, opts, vim.schedule_wrap(idris.world(callback, 2)))"
 ++ "           end"
 ++ "        end"
 ++ "    end"
 ++ "end"

-- TODO: leave schedule_wrap here or generalise even more?
%foreign prim__spawn__ffi
prim__spawn : String -> OpaqueDict -> (Int -> Int -> PrimIO ()) -> PrimIO OpaqueDict

export
spawn : HasIO io
     => String
     -> Dict [("args", Lua.List [String, String]), ("stdio", Lua.List [OpaqueDict, OpaqueDict, OpaqueDict])]
     -> (Int -> Int -> IO ())
     -> io (Dict [("handle", OpaqueDict), ("pid", Int)])
spawn cmd (MkDict opts) f = MkDict <$> primIO (prim__spawn cmd opts (\x, y => toPrim $ f x y))
