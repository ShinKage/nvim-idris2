module Plugin

import Data.List
import Data.Ref
import Data.Strings
import Language.Reflection

import Idris.IDEMode.Commands
import Idris.IDEMode.Parser
import Parser.Lexer.Source
import Parser.Support
import Utils.Hex

import Commands
import Foreign

%language ElabReflection

-- TODO: Move all temporary foreign declaration to vim api module

data Server : Type where
data Client : Type where

%foreign "s, h => {args={'--ide-mode-socket', s}, stdio={nil, h, nil}}"
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

%foreign "_ => vim.fn.getcurpos()"
getCurPos : PrimIO OpaqueDict

%foreign "pos, _ => vim.fn.setpos('.', pos)"
setCurPos : OpaqueDict -> PrimIO ()

%foreign "idris.support.appendToBuffer|support"
appendLines : Bool -> Bool -> Int -> String -> PrimIO ()

%foreign "l, _ => vim.fn.deletebufline('%', l)"
deleteLine : Int -> PrimIO ()

%foreign "s, _ => vim.fn.line(s)"
line : String -> PrimIO Int

%foreign "_ => vim.fn.strftime('%T')"
currenttime : PrimIO String

%foreign "s, _ => vim.fn.bufwinnr(s)"
bufwinnr : String -> PrimIO Int

writeToBuffer : Bool -> String -> IO ()
writeToBuffer commented str = do
  -- TODO: ensure buffer is opened
  cwid <- primIO $ bufwinnr "."
  rwid <- primIO $ bufwinnr "idris-response"
  primIO $ nvimCommand $ show rwid ++ "wincmd w"
  lastline <- primIO $ line "$"
  primIO $ appendLines commented True lastline str
  primIO $ nvimCommand "normal! G"
  primIO $ nvimCommand $ show cwid ++ "wincmd w"
  -- cur <- primIO getCurPos
  -- primIO $ nvimCommand "b idris-response"
  -- lastline <- primIO $ line "$"
  -- primIO $ appendLines commented lastline str
  -- primIO $ nvimCommand "normal! G"
  -- primIO $ nvimCommand "b #"
  -- primIO $ setCurPos cur

splitMessages : String -> List String -> List String
splitMessages recv acc =
  case fromHex (reverse $ substr 0 6 recv) of
       Just 0 => acc
       Just len => let msg = substr 6 (integerToNat $ cast len) recv
                       rest = substr (integerToNat $ cast $ 6 + len) (length recv) recv in
                       splitMessages rest (snoc acc msg)
       Nothing => acc

process : IDEResult -> IO ()
process (OK idx res) = do
  case !(primIO $ getCmdFromHistory idx) of
    Just (Interpret sel) => do
      let SExpList ((StringAtom ls) :: _) = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to Interpret" ++ show res ++ "'"
      writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (LoadFile path _) => do
      writeToBuffer True $ !(primIO currenttime) ++ "\nSuccesfully reloaded " ++ path
      primIO $ deleteCmdInHistory idx
    Just (TypeOf name _) => do
      let SExpList ((StringAtom ls) :: _) = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to TypeOf" ++ show res ++ "'"
      writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (DocsFor name _) => do
      let SExpList ((StringAtom ls) :: _) = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to DocsFor" ++ show res ++ "'"
      writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (CaseSplit line col name) => do
      let SExpList [StringAtom ls] = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to CaseSplit" ++ show res ++ "'"
      primIO $ deleteLine (cast line)
      primIO $ appendLines False False (cast line - 1) ls
      primIO $ deleteCmdInHistory idx
    Just (AddClause line name) => do
      let SExpList [StringAtom ls] = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to AddClause" ++ show res ++ "'"
      primIO $ appendLines False False (cast line) ls
      primIO $ deleteCmdInHistory idx
    Just (ExprSearch line name _ _) => do
      let SExpList [StringAtom ls] = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to ExprSearch" ++ show res ++ "'"
      primIO $ nvimCommand $ "s/" ++ (strCons '?' name) ++ "/" ++ ls ++ "/"
      primIO $ putLastSearch ls
      primIO $ deleteCmdInHistory idx
    Just ExprSearchNext => do
      -- TODO: How we rollback?
      let SExpList [StringAtom ls] = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to ExprSearch" ++ show res ++ "'"
      writeToBuffer True $ "Inline substitution of next expression search is not available\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (GenerateDef line name) => do
      let SExpList [StringAtom ls] = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to GenerateDef" ++ show res ++ "'"
      primIO $ appendLines False False (cast line) ls
      primIO $ deleteCmdInHistory idx
    Just GenerateDefNext => do
      -- TODO: How we rollback?
      let SExpList [StringAtom ls] = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to ExprSearch" ++ show res ++ "'"
      writeToBuffer True $ "Inline substitution of next definition is not available\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (MakeLemma line name) => do
      -- TODO: How we find where to put the lemma? (Maybe, go to the previous paragraph and append there)
      let SExpList [StringAtom ls] = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to ExprSearch" ++ show res ++ "'"
      writeToBuffer False $ "Inline substitution of make lemma is not available\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (MakeCase line name) => do
      -- TODO: Feels like a hack
      let SExpList [StringAtom ls] = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to MakeCase" ++ show res ++ "'"
      primIO $ deleteLine (cast line)
      primIO $ appendLines False False (cast line - 1) ls
      primIO $ deleteCmdInHistory idx
    Just (MakeWith line name) => do
      -- TODO: Feels like a hack
      let SExpList [StringAtom ls] = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to MakeWith" ++ show res ++ "'"
      primIO $ deleteLine (cast line)
      primIO $ appendLines False False (cast line - 1) ls
      primIO $ deleteCmdInHistory idx
    Just (Metavariables _) => do
      -- let SExpList [StringAtom ls] = res
      --   | x => primIO $ nvimCommand $ "echom 'Invalid response to Metavariables" ++ show res ++ "'"
      writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ (show res)
      primIO $ deleteCmdInHistory idx
    Just (BrowseNamespace name) => do
      let SExpList ((StringAtom ls) :: _) = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to BrowseNamespace" ++ show res ++ "'"
      writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (EnableSyntax _) => do
      let SExpList ((StringAtom ls) :: _) = res
        | x => primIO $ nvimCommand $ "echom 'Invalid response to EnableSyntax"++ show res ++ "'"
      writeToBuffer True $ !(primIO currenttime) ++ "\nServer message: \"" ++ ls ++ "\""
      primIO $ deleteCmdInHistory idx
    Just GetOptions => do
      writeToBuffer True (show res)
      primIO $ deleteCmdInHistory idx
    x => pure ()
process (Warning idx (SExpList [StringAtom file, start, end, StringAtom msg, _])) = do
  writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ msg
  primIO $ deleteCmdInHistory idx
process (Error idx (SExpList ((StringAtom ls) :: _))) = do
  writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ ls
  primIO $ deleteCmdInHistory idx
process (WriteString idx res) = do
  let msg = !(primIO currenttime) ++ "\nServer message: \"" ++ res ++ "\""
  writeToBuffer True msg
  primIO $ deleteCmdInHistory idx
process (Output idx res) = primIO $ deleteCmdInHistory idx
process (Version (SExpList [IntegerAtom major, IntegerAtom minor])) = do
  let msg = !(primIO currenttime) ++ "\nIdris2 IDE Mode server version " ++ show major ++ "." ++ show minor
  writeToBuffer True msg
process _ = primIO $ nvimCommand $ "echom 'Invalid message from the server'"

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
                if (head == "(:out")
                   then primIO $ nvimCommand $ "echo 'skipped output'"
                   else do let Right sexp = parseSExp msg
                             | Left err => primIO $ nvimCommand $ "echom 'invalid response: " ++ show err ++ "'"
                           let Just res = getResult sexp
                             | Nothing => primIO $ nvimCommand $ "echom 'invalid response: " ++ show sexp ++ "'"
                           process res)
          (\err => primIO $ nvimCommand $ "echom 'read err: " ++ err ++ "'")
          (pure ()))
    (\err => primIO $ nvimCommand $ "echom 'connect error: " ++ err ++ "'")
  -- write client !(buildCommand $ EnableSyntax False) -- Still not merged
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

%foreign "name, _ => vim.fn.bufexists(name)"
bufexists : String -> PrimIO Int

main : IO ()
main = do
  loadCommands

  serverRef <- newRef Server $ the (Maybe OpaqueDict) Nothing
  clientRef <- newRef Client $ the (Maybe OpaqueDict) Nothing

  autostart <- getGlobalBoolVar "idris2_autostart" True

  when autostart $ do
    primIO $ nvimCommand "set maxfuncdepth=10000"
    primIO $ nvimCommand "echom 'starting idris2 ide mode plugin'"

    externalClientOpt <- getGlobalBoolVar "idris2_external_server" False
    if externalClientOpt
       then do primIO $ nvimCommand "echom 'starting with external server'"
               host <- getGlobalStringVar "idris2_external_host" "127.0.0.1"
               port <- getGlobalIntVar "idris2_external_port" 38398
               client <- connectIdris2 host port
               primIO $ setGlobalClient client
       else do primIO $ nvimCommand "echom 'starting server'"
               port <- getGlobalIntVar "idris2_server_port" 38398
               spawnAndConnectIdris2 ("localhost", port) ("127.0.0.1", port)

    -- KEYBINDINGS
    nnoremap !(getGlobalStringVar "idris2_loadCurrent_key" "<LocalLeader>r")
              (commandBinding True `{{loadCurrent}})
    nnoremap !(getGlobalStringVar "idris2_typeOf_key" "<LocalLeader>t")
              (commandBinding True `{{typeOf}})
    nnoremap !(getGlobalStringVar "idris2_docOverview_key" "<LocalLeader>d")
              (commandBinding True `{{docOverview}})
    nnoremap !(getGlobalStringVar "idris2_caseSplit_key" "<LocalLeader>c")
              (commandBinding True `{{caseSplit}})
    nnoremap !(getGlobalStringVar "idris2_exprSearch_key" "<LocalLeader>s")
              (commandBinding True `{{exprSearch}})
    nnoremap !(getGlobalStringVar "idris2_exprSearchNext_key" "<LocalLeader>sn")
              (commandBinding True `{{exprSearchNext}})
    nnoremap !(getGlobalStringVar "idris2_addClause_key" "<LocalLeader>a")
              (commandBinding True `{{addClause}})
    nnoremap !(getGlobalStringVar "idris2_generateDef_key" "<LocalLeader>g")
              (commandBinding True `{{generateDef}})
    nnoremap !(getGlobalStringVar "idris2_generateDefNext_key" "<LocalLeader>gn")
              (commandBinding True `{{generateDefNext}})
    nnoremap !(getGlobalStringVar "idris2_makeLemma_key" "<LocalLeader>l")
              (commandBinding True `{{makeLemma}})
    nnoremap !(getGlobalStringVar "idris2_makeCase_key" "<LocalLeader>mc")
              (commandBinding True `{{makeCase}})
    nnoremap !(getGlobalStringVar "idris2_makeWith_key" "<LocalLeader>w")
              (commandBinding True `{{makeWith}})
    vnoremap !(getGlobalStringVar "idris2_interpret_key" "<LocalLeader>e")
              (commandBinding True `{{interpret}}) -- evaluates selected code

    -- AUTOCOMMANDS
    loadOnStart <- getGlobalBoolVar "idris2_load_on_start" True
    when loadOnStart $ do
      primIO $ nvimCommand "augroup IdrisIDE"
      primIO $ nvimCommand $ "autocmd BufNewFile,BufRead *.idr " ++ commandBinding False `{{loadCurrent}}
      primIO $ nvimCommand "augroup end"

    -- RESPONSE BUFFER
    primIO $ nvimCommand "vertical rightbelow split"
    primIO $ nvimCommand "badd idris-response"
    primIO $ nvimCommand "b idris-response"
    primIO $ nvimCommand "set buftype=nofile"
    primIO $ nvimCommand "wincmd h"
