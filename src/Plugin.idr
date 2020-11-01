module Plugin

import Data.List
import Data.Strings
import Data.Ref
import Language.Reflection

import Parser.Lexer.Source
import Parser.Support
import Idris.IDEMode.Commands
import Idris.IDEMode.Parser
import Utils.Hex

import Foreign
import Commands

%language ElabReflection

-- TODO: Move all temporary foreign declaration to vim api module

data Server : Type where
data Client : Type where

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

writeToBuffer : String -> IO ()
writeToBuffer str = do
  cur <- primIO getCurPos
  primIO $ nvimCommand "b idris-response"
  lastline <- primIO $ line "$"
  primIO $ appendLines lastline str
  primIO $ nvimCommand "b #"
  primIO $ setCurPos cur

splitMessages : String -> List String -> List String
splitMessages recv acc =
  case fromHex (reverse $ substr 0 6 recv) of
       Just 0 => acc
       Just len => let msg = substr 6 (integerToNat $ cast len) recv
                       rest = substr (integerToNat $ cast $ 6 + len) (length recv) recv in
                       splitMessages rest (snoc acc msg)
       Nothing => acc

connectIdris2 : String -> Int -> IO OpaqueDict
connectIdris2 host port = do
  primIO $ nvimCommand "echom 'Starting connection...'"
  client <- newTCP
  primIO $ nvimCommand "echom 'Created new tcp socket...'"
  connect client host port
    (do readStart client
          (\recv => do
              let msgs = splitMessages recv []
              for_ msgs $ \msg => do
                let head = substr 0 5 msg
                let Right sexp = parseSExp msg
                  | Left err => primIO $ nvimCommand $ "echom 'invalid response: " ++ show err ++ "'"
                let Just res = getResult sexp
                  | Nothing => primIO $ nvimCommand $ "echom 'invalid response: " ++ show sexp ++ "'"
                writeToBuffer (show res))
          (\err => primIO $ nvimCommand $ "echom 'read err: " ++ err ++ "'")
          (pure ()))
    (\err => primIO $ nvimCommand $ "echom 'connect error: " ++ err ++ "'")
  write client (buildCommand $ EnableSyntax False) -- Still not merged
  pure client

quitServer : HasIO io
          => Ref Server (Maybe OpaqueDict)
          => io ()
quitServer = do
  Just handle <- get Server
    | Nothing => pure ()
  sigterm handle
  put Server Nothing

-- TODO: Use IORef instead of an hacky global variable. How we pass it to
--       callbacks without loosing generality?
spawnAndConnectIdris2 : HasIO io
                     => Ref Server (Maybe OpaqueDict)
                     => Ref Client (Maybe OpaqueDict)
                     => (String, Int)
                     -> (String, Int)
                     -> io ()
spawnAndConnectIdris2 (shost, sport) (chost, cport) = do
  Nothing <- get Server
    | Just _ => primIO $ nvimCommand "echom 'Idris2 process already spawned'"
  stdout <- newPipe
  let opts = spawnOpts (shost ++ ":" ++ show sport) stdout
  d <- spawn "idris2" opts (\code, signal => primIO $ nvimCommand $ "echom 'Idris2 process closed with code " ++ show code ++ "'")
  let handle : OpaqueDict = getField d "handle"
  put Server (Just handle)
  readStart stdout (\msg => do primIO $ nvimCommand $ "echom 'server: " ++ msg ++ "'"
                               if trim msg == show sport
                                  then do client <- connectIdris2 chost cport
                                          primIO $ setGlobalClient client -- TODO: can we remove it?
                                          put Client (Just client)
                                          readStop stdout
                                  else do primIO $ nvimCommand $ "echom 'wrong port " ++ msg ++ "'"
                                          quitServer)
                   (\err => do primIO $ nvimCommand $ "echom 'err: " ++ err ++ "'"
                               quitServer)
                   (do primIO $ nvimCommand "echom 'Idris2 stdout closed'"
                       quitServer)

%foreign "function(key, cmd) vim.api.nvim_set_keymap('n', key, cmd, { noremap = true, silent = true }) end"
nnoremap : String -> String -> PrimIO ()

-- FIXME: Hack to call arbitrary Idris2 functions from lua, required for
--        keybindings
export
unsafeWriteHack : ()
unsafeWriteHack = unsafePerformIO $ do
  client <- primIO getGlobalClient
  let cmd = LoadFile "Test.idr" Nothing
  write client (buildCommand cmd) -- "00001b((:load-file \"Test.idr\") 1)"

%foreign "function(name) return vim.fn.bufexists(name) end"
bufexists : String -> PrimIO Int

%foreign "function(type) vim.bo.buftype = type end"
setBuftype : String -> PrimIO ()

main : IO ()
main = do
  loadCommands

  -- serverRef <- newRef Server Nothing
  -- clientRef <- newRef Client Nothing

  primIO $ nvimCommand "set maxfuncdepth=10000"
  primIO $ nvimCommand "echom 'starting idris2 plugin'"
  client <- connectIdris2 "127.0.0.1" 38398
  primIO $ setGlobalClient client
  -- spawnAndConnectIdris2 ("localhost", 38398) ("127.0.0.1", 38398)
  primIO $ nnoremap "<Leader>r" $ commandBinding `{{loadCurrent}}
  primIO $ nnoremap "<Leader>t" $ commandBinding `{{typeOf}}
  primIO $ nnoremap "<Leader>d" $ commandBinding `{{docOverview}}
  primIO $ nnoremap "<Leader>c" $ commandBinding `{{caseSplit}}
  primIO $ nnoremap "<Leader>s" $ commandBinding `{{exprSearch}}
  primIO $ nnoremap "<Leader>sn" $ commandBinding `{{exprSearchNext}}
  primIO $ nnoremap "<Leader>d" $ commandBinding `{{generateDef}}
  primIO $ nnoremap "<Leader>dn" $ commandBinding `{{generateDefNext}}
  primIO $ nnoremap "<Leader>l" $ commandBinding `{{makeLemma}}
  primIO $ nnoremap "<Leader>w" $ commandBinding `{{makeWith}}
  primIO $ nvimCommand "vertical rightbelow split"
  primIO $ nvimCommand "badd idris-response"
  primIO $ nvimCommand "b idris-response"
  primIO $ setBuftype "nofile"
  primIO $ nvimCommand "wincmd h"
