module Foreign

import Idris.IDEMode.Commands

import public System.FFI.Lua

-- TODO: Build safe wrappers for sockets and handlers `OpaqueDict`s
-- TODO: PrimIO exposed foreigns are not finalised designs

%foreign "function(_) return vim.loop.new_pipe(false) end"
prim__newPipe : PrimIO OpaqueDict

export
newPipe : HasIO io => io OpaqueDict
newPipe = primIO prim__newPipe

%foreign "function(_) return vim.loop.new_tcp() end"
prim__newTCP : PrimIO OpaqueDict

export
newTCP : HasIO io => io OpaqueDict
newTCP = primIO prim__newTCP

%foreign "idris.support.spawn|support"
prim__spawn : String -> OpaqueDict -> (Int -> Int -> PrimIO ()) -> PrimIO OpaqueDict

export
spawn : HasIO io
     => String
     -> Dict [("args", Lua.List [String, String]), ("stdio", Lua.List [OpaqueDict, OpaqueDict, OpaqueDict])]
     -> (Int -> Int -> IO ())
     -> io (Dict [("handle", OpaqueDict), ("pid", Int)])
spawn cmd (MkDict opts) f =
  MkDict <$> primIO (prim__spawn cmd opts (\x, y => toPrim $ f x y))

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

%foreign "function(stream) return function(_) stream:read_stop() end end"
prim__readStop : OpaqueDict -> PrimIO ()

export
readStop : HasIO io => OpaqueDict -> io ()
readStop = primIO . prim__readStop

%foreign "function(handle) return function(_) handle:kill(15) end end"
prim__sigtermHandle : OpaqueDict -> PrimIO ()

export
sigterm : HasIO io => OpaqueDict -> io ()
sigterm = primIO . prim__sigtermHandle

%foreign "function(client) return function(_) client:shutdown() end end"
prim__shutdown : OpaqueDict -> PrimIO ()

export
shutdown : HasIO io => OpaqueDict -> io ()
shutdown = primIO . prim__shutdown

%foreign "function(client) return function(_) client:close() end end"
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

%foreign "function(client) return function(data) return function(_) client:write(data) end end end"
prim__write : OpaqueDict -> String -> PrimIO ()

export
write : HasIO io => OpaqueDict -> String -> io ()
write client s = primIO $ prim__write client s

%foreign "function(_) return vim.fn.expand('%:p') end"
prim__filePath : PrimIO String

export
filePath : HasIO io => io String
filePath = primIO prim__filePath

-- FIXME: Lua global variable hack
export %foreign "function(client) return function(_) global_client=client end end"
setGlobalClient : OpaqueDict -> PrimIO ()

export %foreign "function(_) return global_client end"
getGlobalClient : PrimIO OpaqueDict

export %foreign "idris.support.putCmdInHistory|support"
putCmdInHistory : Int -> IDECommand -> PrimIO Int

export %foreign "idris.support.putLastSearch|support"
putLastSearch : String -> PrimIO Int

export %foreign "idris.support.deleteCmdInHistory|support"
deleteCmdInHistory : Int -> PrimIO ()

export %foreign "idris.support.deleteLastSearch|support"
deleteLastSearch : Int -> PrimIO ()

export %foreign "idris.support.getCmdFromHistory|support"
getCmdFromHistory : Int -> PrimIO (Maybe IDECommand)

export %foreign "idris.support.getLastSearch|support"
getLastSearch : PrimIO (Maybe String)

export %foreign "idris.support.genHistoryIndex|support"
genHistoryIndex : PrimIO Int

export %foreign "idris.support.fastLines|support"
fastLines : String -> List String

%foreign "function(_) return vim.fn.expand('<cword>') end"
prim__cursorWord : PrimIO String

export
cursorWord : HasIO io => io String
cursorWord = primIO prim__cursorWord

%foreign "function(_) return vim.fn.expand('<cWORD>') end"
prim__cursorWord' : PrimIO String

export
cursorWord' : HasIO io => io String
cursorWord' = primIO prim__cursorWord'

%foreign "function(_) return vim.fn.line('.') end"
prim__cursorLine : PrimIO Int

export
cursorLine : HasIO io => io Int
cursorLine = primIO prim__cursorLine

%foreign "function(_) return vim.fn.col('.') end"
prim__cursorColumn : PrimIO Int

export
cursorColumn : HasIO io => io Int
cursorColumn = primIO prim__cursorColumn

%foreign "function(line) return function(col) return function(_) vim.fn.cursor(line, col) end end end"
prim__setCursor : Int -> Int -> PrimIO ()

export
setCursor : HasIO io => Int -> Int -> io ()
setCursor l c = primIO $ prim__setCursor l c

%foreign "function(pat) return function(_) return vim.fn.search(pat) end end"
prim__searchPattern : String -> PrimIO Int

export
searchPattern : HasIO io => String -> io Int
searchPattern pat = primIO $ prim__searchPattern pat

%foreign "idris.support.getSelection|support"
prim__getSelection : PrimIO String

export
getSelection : HasIO io => io String
getSelection = primIO prim__getSelection

%foreign "function(_) vim.api.nvim_command('w') end"
prim__saveBuffer : PrimIO ()

export
saveBuffer : HasIO io => io ()
saveBuffer = primIO prim__saveBuffer

%foreign "function(_) return vim.bo.modified end"
prim__isBufferModified : PrimIO String

export
isBufferModified : HasIO io => io Bool
isBufferModified = case !(primIO prim__isBufferModified) of
                        "true" => pure True
                        _ => pure False

export %foreign "function(cmd) return function(_) vim.api.nvim_command(cmd) end end"
nvimCommand : String -> PrimIO ()

%foreign "function(key) return function(cmd) return function(_) vim.api.nvim_set_keymap('n', key, cmd, { noremap = true, silent = true }) end end end"
prim__nnoremap : String -> String -> PrimIO ()

export
nnoremap : HasIO io => String -> String -> io ()
nnoremap k cmd = primIO $ prim__nnoremap k cmd
