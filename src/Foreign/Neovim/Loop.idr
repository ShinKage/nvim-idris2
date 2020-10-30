module Foreign.Neovim.Loop

import public System.FFI.Lua

%default total

export
data Stream : Type where
  MkStream : OpaqueDict -> Stream

%foreign "function() return vim.loop.new_pipe(false) end"
prim__newPipe : PrimIO OpaqueDict

export
newPipe : HasIO io => io Stream
newPipe = MkStream <$> primIO prim__newPipe

export
data Socket : Type where
  MkSocket : OpaqueDict -> Socket

%foreign "function() return vim.loop.new_tcp() end"
prim__newTCP : PrimIO OpaqueDict

export
newTCP : HasIO io => io Socket
newTCP = MkSocket <$> primIO prim__newTCP

-- FIXME: scheme backend properly manages PrimIO callbacks without having to
--        pass around the world token in the foreign implementation, can we do
--        the same in the lua backend? Currently hacked to unsafe "pure"
--        callbacks.
-- TODO: leave schedule_wrap here or generalise even more?
%foreign "function(cmd, opts, callback) return vim.loop.spawn(cmd, opts, vim.schedule_wrap(callback)) end"
prim__spawn : String -> OpaqueDict -> (Int -> Int -> ()) -> PrimIO OpaqueDict

-- FIXME: unsafePerformIO for the hack described in prim__spawn.
export
spawn : HasIO io
     => String
     -> Dict [("args", Lua.List [String, String]), ("stdio", Lua.List [OpaqueDict, OpaqueDict, OpaqueDict])]
     -> (Int -> Int -> IO ())
     -> io (Dict [("handle", OpaqueDict), ("pid", Int)])
spawn cmd (MkDict opts) f = MkDict <$> primIO (prim__spawn cmd opts (\x, y => unsafePerformIO $ f x y))
