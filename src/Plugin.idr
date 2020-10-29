module Plugin

import Parser.Lexer.Source
import Parser.Support
import Idris.IDEMode.Commands
import Idris.IDEMode.Parser
import Data.Strings

import Foreign

-- TODO: Move all temporary foreign declaration to vim api module

%foreign "function(s) print(s) end"
luaPrint : String -> PrimIO ()

%foreign "function(cmd) vim.api.nvim_command(cmd) end"
nvimCommand : String -> PrimIO ()

%foreign "function(s, h) return {args={'--ide-mode-socket', s}, stdio={nil, h, nil}} end"
prim__spawnOpts : String -> OpaqueDict -> OpaqueDict

spawnOpts : String
         -> OpaqueDict
         -> Dict [("args", Lua.List [String, String]), ("stdio", Lua.List [OpaqueDict, OpaqueDict, OpaqueDict])]
spawnOpts s h = MkDict $ prim__spawnOpts s h

spawnIdris2 : String -> Int
           -> IO (OpaqueDict, OpaqueDict)
spawnIdris2 host port = do
  stdout <- newPipe
  let opts = spawnOpts (host ++ ":" ++ show port) stdout
  d <- spawn "idris2" opts (\code, signal => primIO $ nvimCommand $ "echom 'Idris2 process closed with code " ++ show code ++ "'")
  let handle : OpaqueDict = getField d "handle"
  readStart stdout (\msg => primIO $ nvimCommand $ "echom 'server: " ++ msg ++ "'")
                   (\err => primIO $ nvimCommand $ "echom 'err: " ++ err ++ "'")
                   (primIO $ nvimCommand  "echom 'Idris2 stdout closed'")
  pure (handle, stdout)

%foreign "function() vim.fn.getcurpos() end"
getCurPos : PrimIO OpaqueDict

%foreign "function(pos) vim.fn.setpos('.', pos) end"
setCurPos : OpaqueDict -> PrimIO ()

%foreign "function(l, s) vim.fn.append(l, vim.split(s, '\\n')) end"
appendLines : Int -> String -> PrimIO ()

%foreign "function(s) return vim.fn.line(s) end"
line : String -> PrimIO Int

connectIdris2 : String -> Int
             -> IO OpaqueDict
connectIdris2 host port = do
  primIO $ nvimCommand "echom 'Starting connection...'"
  client <- newTCP
  primIO $ nvimCommand "echom 'Created new tcp socket...'"
  connect client host port (do
    readStart client (\msg => do
                         cur <- primIO getCurPos
                         primIO $ nvimCommand "b idris-response"
                         -- primIO $ nvimCommand "%delete"
                         lastline <- primIO $ line "$"
                         primIO $ appendLines lastline msg
                         primIO $ nvimCommand "b #"
                         primIO $ setCurPos cur)
                     (\err => primIO $ nvimCommand $ "echom 'read err: " ++ err ++ "'")
                     (pure ()))
    (\err => primIO $ nvimCommand $ "echom 'connect error: " ++ err ++ "'")
  pure client

-- FIXME: Lua global variable hack
%foreign "function(client) global_client=client end"
setGlobalClient : OpaqueDict -> PrimIO ()

%foreign "function() return global_client end"
getGlobalClient : PrimIO OpaqueDict

-- TODO: Use IORef instead of an hacky global variable. How we pass it to
--       callbacks without loosing generality?
spawnAndConnectIdris2 : (String, Int) -> (String, Int) -> IO OpaqueDict
spawnAndConnectIdris2 (shost, sport) (chost, cport) = do
  stdout <- newPipe
  let opts = spawnOpts (shost ++ ":" ++ show sport) stdout
  d <- spawn "idris2" opts (\code, signal => primIO $ nvimCommand $ "echom 'Idris2 process closed with code " ++ show code ++ "'")
  let handle : OpaqueDict = getField d "handle"
  readStart stdout (\msg => do primIO $ nvimCommand $ "echom 'server: " ++ msg ++ "'"
                               if trim msg == show sport
                                  then do client <- connectIdris2 chost cport
                                          primIO $ setGlobalClient client
                                          readStop stdout
                                  else do primIO $ nvimCommand $ "echom 'wrong port " ++ msg ++ "'"
                                          sigterm handle)
                   (\err => do primIO $ nvimCommand $ "echom 'err: " ++ err ++ "'"
                               sigterm handle)
                   (do primIO $ nvimCommand "echom 'Idris2 stdout closed'"
                       sigterm handle)
  pure handle


%foreign "function(key, cmd) vim.api.nvim_set_keymap('n', key, cmd, { noremap = true, silent = true }) end"
nnoremap : String -> String -> PrimIO ()

-- FIXME: Hack to call arbitrary Idris2 functions from lua, required for
--        keybindings
export
unsafeWriteHack : ()
unsafeWriteHack = unsafePerformIO $ do
  client <- primIO getGlobalClient
  write client "00001b((:load-file \"Test.idr\") 1)"

%foreign "function(name) return vim.fn.bufexists(name) end"
bufexists : String -> PrimIO Int

%foreign "function(type) vim.bo.buftype = type end"
setBuftype : String -> PrimIO ()

main : IO ()
main = do
  -- FIXME: Hack to guarantee that the compiler puts this function in the
  --        compiled output
  let () = if False then unsafeWriteHack else ()

  primIO $ nvimCommand "echom 'starting idris2 plugin'"
  -- client <- connectIdris2 "127.0.0.1" 38398
  handle <- spawnAndConnectIdris2 ("localhost", 38398) ("127.0.0.1", 38398)
  primIO $ nnoremap "<Leader>L" ":lua idris[\"Plugin.unsafeWriteHack\"]()<CR>"
  primIO $ nvimCommand "vertical rightbelow split"
  primIO $ nvimCommand "badd idris-response"
  primIO $ nvimCommand "b idris-response"
  primIO $ setBuftype "nofile"
  primIO $ nvimCommand "wincmd h"
