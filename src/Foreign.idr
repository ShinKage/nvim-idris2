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

-- FIXME: Previous hacks plus (() -> ()) to pass the callback lazily, otherwise
--        the unsafePerformIO would force evaluation before reaching the
--        function.
%foreign "function(stream, onok, onerr, onclose) stream:read_start(vim.schedule_wrap(function (err, chunk) if err then onerr(err) elseif chunk then onok(chunk) else onclose(nil) end end)) end"
prim__readStart : OpaqueDict -> (String -> ()) -> (String -> ()) -> (() -> ()) -> PrimIO ()

export
readStart : HasIO io => OpaqueDict
         -> (onok : String -> IO ())
         -> (onerr : String -> IO ())
         -> (onclose : IO ())
         -> io ()
readStart stream onok onerr onclose = primIO $ prim__readStart stream (\s => unsafePerformIO $ onok s) (\s => unsafePerformIO $ onerr s) (\_ => unsafePerformIO onclose)

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

%foreign "function(client, host, port, onok, onerr) client:connect(host, port, function (err) if err then onerr(err) else onok(nil) end end) end"
prim__connect : OpaqueDict -> String -> Int -> (() -> ()) -> (String -> ()) -> PrimIO ()

export
connect : HasIO io
       => OpaqueDict
       -> String
       -> Int
       -> IO ()
       -> (String -> IO ())
       -> io ()
connect client host port onok onerr = primIO $ prim__connect client host port (\_ => unsafePerformIO onok) (\s => unsafePerformIO $ onerr s)

%foreign "function(client, data) client:write(data) end"
prim__write : OpaqueDict -> String -> PrimIO ()

export
write : HasIO io => OpaqueDict -> String -> io ()
write client s = primIO $ prim__write client s
