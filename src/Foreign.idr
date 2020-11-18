module Foreign

import Data.Maybe

import Idris.IDEMode.Commands

import public System.FFI.Lua

-- TODO: Build safe wrappers for sockets and handlers `OpaqueDict`s
-- TODO: PrimIO exposed foreigns are not finalised designs

%foreign "_ => vim.loop.new_pipe(false)"
prim__newPipe : PrimIO OpaqueDict

export
newPipe : HasIO io => io OpaqueDict
newPipe = primIO prim__newPipe

%foreign "_ => vim.loop.new_tcp()"
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

%foreign "stream, _ => stream:read_stop()"
prim__readStop : OpaqueDict -> PrimIO ()

export
readStop : HasIO io => OpaqueDict -> io ()
readStop = primIO . prim__readStop

%foreign "handle, _ => handle:kill(15)"
prim__sigtermHandle : OpaqueDict -> PrimIO ()

export
sigterm : HasIO io => OpaqueDict -> io ()
sigterm = primIO . prim__sigtermHandle

%foreign "client, _ => client:shutdown()"
prim__shutdown : OpaqueDict -> PrimIO ()

export
shutdown : HasIO io => OpaqueDict -> io ()
shutdown = primIO . prim__shutdown

%foreign "client, _ => client:close()"
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

%foreign "client, data, _ => client:write(data)"
prim__write : OpaqueDict -> String -> PrimIO ()

export
write : HasIO io => OpaqueDict -> String -> io ()
write client s = primIO $ prim__write client s

%foreign "_ => vim.fn.expand('%:p')"
prim__filePath : PrimIO String

export
filePath : HasIO io => io String
filePath = primIO prim__filePath

-- FIXME: Lua global variable hack
export %foreign "client => function(_) global_client = client end"
setGlobalClient : OpaqueDict -> PrimIO ()

export %foreign "_ => global_client"
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

%foreign "_ => vim.fn.expand('<cword>')"
prim__cursorWord : PrimIO String

export
cursorWord : HasIO io => io String
cursorWord = primIO prim__cursorWord

%foreign "_ => vim.fn.expand('<cWORD>')"
prim__cursorWord' : PrimIO String

export
cursorWord' : HasIO io => io String
cursorWord' = primIO prim__cursorWord'

%foreign "_ => vim.fn.line('.')"
prim__cursorLine : PrimIO Int

export
cursorLine : HasIO io => io Int
cursorLine = primIO prim__cursorLine

%foreign "_ => vim.fn.col('.')"
prim__cursorColumn : PrimIO Int

export
cursorColumn : HasIO io => io Int
cursorColumn = primIO prim__cursorColumn

%foreign "line, col, _ => vim.fn.cursor(line, col)"
prim__setCursor : Int -> Int -> PrimIO ()

export
setCursor : HasIO io => Int -> Int -> io ()
setCursor l c = primIO $ prim__setCursor l c

%foreign "pat, _ => vim.fn.search(pat)"
prim__searchPattern : String -> PrimIO Int

export
searchPattern : HasIO io => String -> io Int
searchPattern pat = primIO $ prim__searchPattern pat

%foreign "idris.support.getSelection|support"
prim__getSelection : PrimIO String

export
getSelection : HasIO io => io String
getSelection = primIO prim__getSelection

%foreign "_ => vim.api.nvim_command('w')"
prim__saveBuffer : PrimIO ()

%foreign "name, _ => vim.fn.bufexists(name)"
prim__bufexists : String -> PrimIO Int

export
bufexists : HasIO io => String -> io Bool
bufexists bufName = do
  case !(primIO $ prim__bufexists bufName) of
    0 => pure False
    _ => pure True

export
saveBuffer : HasIO io => io ()
saveBuffer = primIO prim__saveBuffer

%foreign "idris.support.isBufferModified|support"
prim__isBufferModified : PrimIO Bool

export
isBufferModified : HasIO io => io Bool
isBufferModified = primIO prim__isBufferModified

%foreign "cmd, _ => vim.api.nvim_command(cmd)"
prim__nvimCommand : String -> PrimIO ()

%foreign "expr, _ => vim.api.nvim_eval(expr)"
prim__nvimEval : String -> PrimIO ty

export
nvimCommand : HasIO io => String -> io ()
nvimCommand = primIO . prim__nvimCommand

export
nvimEval : HasIO io => String -> io ty
nvimEval = primIO . prim__nvimEval

%foreign "key, cmd, _ => vim.api.nvim_set_keymap('n', key, cmd, { noremap = true, silent = true })"
prim__nnoremap : String -> String -> PrimIO ()

%foreign "key, cmd, _ => vim.api.nvim_set_keymap('v', key, cmd, { noremap = true, silent = true })"
prim__vnoremap : String -> String -> PrimIO ()

export
nnoremap : HasIO io => String -> String -> io ()
nnoremap k cmd = primIO $ prim__nnoremap k cmd

export
vnoremap : HasIO io => String -> String -> io ()
vnoremap k cmd = primIO $ prim__vnoremap k cmd

%foreign "idris.support.getGlobalStringVar|support"
prim__getGlobalStringVar : String -> PrimIO (Maybe String)

export
getGlobalStringVar : HasIO io => String -> Lazy String -> io String
getGlobalStringVar key def = do
  val <- primIO $ prim__getGlobalStringVar key
  pure $ fromMaybe def val

export
getGlobalStringVar' : HasIO io => String -> io (Maybe String)
getGlobalStringVar' key = primIO $ prim__getGlobalStringVar key

%foreign "idris.support.getGlobalBoolVar|support"
prim__getGlobalBoolVar : String -> PrimIO (Maybe Bool)

export
getGlobalBoolVar : HasIO io => String -> Lazy Bool -> io Bool
getGlobalBoolVar key def = do
  val <- primIO $ prim__getGlobalBoolVar key
  pure $ fromMaybe def val

export
getGlobalBoolVar' : HasIO io => String -> io (Maybe Bool)
getGlobalBoolVar' key = primIO $ prim__getGlobalBoolVar key

%foreign "idris.support.getGlobalIntVar|support"
prim__getGlobalIntVar : String -> PrimIO (Maybe Int)

export
getGlobalIntVar : HasIO io => String -> Lazy Int -> io Int
getGlobalIntVar key def = do
  val <- primIO $ prim__getGlobalIntVar key
  pure $ fromMaybe def val

export
getGlobalIntVar' : HasIO io => String -> io (Maybe Int)
getGlobalIntVar' key = primIO $ prim__getGlobalIntVar key
