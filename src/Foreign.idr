module Foreign

import public System.FFI.Lua

-- TODO: Build safe wrappers for sockets and handlers `OpaqueDict`s

%foreign "function() return vim.loop.new_pipe(false) end"
prim__newPipe : PrimIO OpaqueDict

export
newPipe : HasIO io => io OpaqueDict
newPipe = primIO prim__newPipe

%foreign "function() return vim.loop.new_tcp() end"
prim__newTCP : PrimIO OpaqueDict

export
newTCP : HasIO io => io OpaqueDict
newTCP = primIO prim__newTCP

-- FIXME: scheme backend properly manages PrimIO callbacks without having to
--        pass around the world token in the foreign implementation, can we do
--        the same in the lua backend? Currently hacked to unsafe "pure"
--        callbacks.
-- TODO: leave schedule_wrap here or generalise even more?
%foreign "idris.support.spawn|support"
prim__spawn : String -> OpaqueDict -> (Int -> Int -> PrimIO ()) -> PrimIO OpaqueDict

-- FIXME: unsafePerformIO for the hack described in prim__spawn.
export
spawn : HasIO io
     => String
     -> Dict [("args", Lua.List [String, String]), ("stdio", Lua.List [OpaqueDict, OpaqueDict, OpaqueDict])]
     -> (Int -> Int -> IO ())
     -> io (Dict [("handle", OpaqueDict), ("pid", Int)])
spawn cmd (MkDict opts) f =
  MkDict <$> primIO (prim__spawn cmd opts (\x, y => toPrim $ f x y))

-- FIXME: Previous hacks plus (() -> ()) to pass the callback lazily, otherwise
--        the unsafePerformIO would force evaluation before reaching the
--        function.
%foreign "idris.support.readStart|support"
prim__readStart : OpaqueDict -> (String -> PrimIO ()) -> (String -> PrimIO ()) -> PrimIO () -> PrimIO ()

export
readStart : HasIO io => OpaqueDict
         -> (onok : String -> IO ())
         -> (onerr : String -> IO ())
         -> (onclose : IO ())
         -> io ()
readStart stream onok onerr onclose =
  primIO $ prim__readStart stream (\s => toPrim $ onok s)
                                  (\s => toPrim $ onerr s)
                                  (toPrim onclose)

%foreign "function(stream) stream:read_stop() end"
prim__readStop : OpaqueDict -> PrimIO ()

export
readStop : HasIO io => OpaqueDict -> io ()
readStop = primIO . prim__readStop

%foreign "function(handle) handle:kill(15) end"
prim__sigtermHandle : OpaqueDict -> PrimIO ()

export
sigterm : HasIO io => OpaqueDict -> io ()
sigterm = primIO . prim__sigtermHandle

%foreign "function(client) client:shutdown() end"
prim__shutdown : OpaqueDict -> PrimIO ()

export
shutdown : HasIO io => OpaqueDict -> io ()
shutdown = primIO . prim__shutdown

%foreign "function(client) client:close() end"
prim__close : OpaqueDict -> PrimIO ()

export
close : HasIO io => OpaqueDict -> io ()
close = primIO . prim__close

%foreign "idris.support.connect|support"
prim__connect : OpaqueDict -> String -> Int -> PrimIO () -> (String -> PrimIO ()) -> PrimIO ()

export
connect : HasIO io
       => OpaqueDict
       -> String
       -> Int
       -> IO ()
       -> (String -> IO ())
       -> io ()
connect client host port onok onerr = primIO $ prim__connect client host port (toPrim onok) (\s => toPrim $ onerr s)

%foreign "function(client, data) client:write(data) end"
prim__write : OpaqueDict -> String -> PrimIO ()

export
write : HasIO io => OpaqueDict -> String -> io ()
write client s = primIO $ prim__write client s

%foreign "function() return vim.fn.expand('%:p') end"
prim__filePath : PrimIO String

export
filePath : HasIO io => io String
filePath = primIO prim__filePath

-- FIXME: Lua global variable hack
export %foreign "function(client) global_client=client end"
setGlobalClient : OpaqueDict -> PrimIO ()

export %foreign "function() return global_client end"
getGlobalClient : PrimIO OpaqueDict

export %foreign "idris.support.fastLines|support"
fastLines : String -> List String

%foreign "function() return vim.fn.expand('<cword>') end"
prim__cursorWord : PrimIO String

export
cursorWord : HasIO io => io String
cursorWord = primIO prim__cursorWord

%foreign "function() return vim.fn.expand('<cWORD>') end"
prim__cursorWord' : PrimIO String

export
cursorWord' : HasIO io => io String
cursorWord' = primIO prim__cursorWord'

%foreign "function() return vim.fn.line('.') end"
prim__cursorLine : PrimIO Int

export
cursorLine : HasIO io => io Int
cursorLine = primIO prim__cursorLine

%foreign "function() return vim.fn.col('.') end"
prim__cursorColumn : PrimIO Int

export
cursorColumn : HasIO io => io Int
cursorColumn = primIO prim__cursorColumn
