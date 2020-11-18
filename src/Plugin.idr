module Plugin

import Data.List
import Data.List1
import Data.Ref
import Data.Strings

import System.File
import System

import Language.Reflection

import Idris.IDEMode.Commands
import Idris.IDEMode.Parser
import Parser.Lexer.Source
import Parser.Support
import Utils.Hex
import Utils.Path

import Commands
import Foreign

%language ElabReflection

-- TODO: Move all temporary foreign declaration to vim api module

data Server : Type where
data Client : Type where

public export
data Parameters : Type where
  [noHints] -- Auto search won't be able to construct instances of this type
            -- straight through this constructor anymore
  MkParameters : (sourceDir : String) -> Parameters

namespace Parameters
  public export
  sourceDir : Parameters -> String
  sourceDir (MkParameters srcDir) = srcDir

  public export
  (.sourceDir) : Parameters -> String
  (.sourceDir) = sourceDir


%foreign "s, h => {args={'-p', 'contrib', '--ide-mode-socket', s}, stdio={nil, h, nil}}"
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
  d <- spawn "idris2" opts (\code, signal => nvimCommand $ "echom 'Idris2 process closed with code " ++ show code ++ "'")
  let handle : OpaqueDict = getField d "handle"
  readStart stdout (\msg => nvimCommand $ "echom 'server: " ++ msg ++ "'")
                   (\err => nvimCommand $ "echom 'err: " ++ err ++ "'")
                   (nvimCommand  "echom 'Idris2 stdout closed'")
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

%foreign "idris.support.fzfWindowBottom"
fzfWindowBottom : (name : String)
               -> (options : List String)
               -> (fn : String -> IO ())
               -> IO ()

%foreign "idris.support.matchlist"
matchlist : (str : String) -> (pattern : String) -> List String

responseBufferName : String
responseBufferName = "idris-response"

trimNewLineAtEnd : (str : String) -> String
trimNewLineAtEnd str =
  fastPack $ reverse $ dropWhile (== '\n') (reverse (fastUnpack str))

||| Runs a system command to find out where
||| the package source files are located. By default,
||| it uses a subdirectory of the local Idris 2 installation
getSourceDirectory : IO String
getSourceDirectory = do
  let cmd = "idris2 --libdir"
  Right fh <- popen cmd Read
      | Left err => do printLn err
                       exitFailure
  Right output <- fGetLine fh
      | Left err => do printLn err
                       exitFailure
  pclose fh
  let path ::: _ = split (== '\n') output
  let pathWithSuffix = path ++ "-src"
  pure pathWithSuffix

||| Creates a response buffer if absent
createResponseBuffer : IO ()
createResponseBuffer =
  when (not !(bufexists responseBufferName)) $
    do nvimCommand "let prevBuf = bufnr(@#)"
       nvimCommand $ "badd " ++ responseBufferName
       nvimCommand $ "b " ++ responseBufferName
       nvimCommand "set buftype=nofile"
       nvimCommand "b #"
       nvimCommand "let @# = prevBuf"

||| Writes to the response buffer, if loaded in one of the windows
writeToBuffer : Bool -> String -> IO ()
writeToBuffer commented str = do
  createResponseBuffer
  cwid <- primIO $ bufwinnr "."
  rwid <- primIO $ bufwinnr responseBufferName
  when (rwid > 0) $ do
   nvimCommand $ show rwid ++ "wincmd w"
   lastline <- primIO $ line "$"
   primIO $ appendLines commented True lastline str
   nvimCommand "normal! G"
   nvimCommand $ show cwid ++ "wincmd w"

splitMessages : String -> List String -> List String
splitMessages recv acc =
  case fromHex (reverse $ substr 0 6 recv) of
       Just 0 => acc
       Just len => let msg = substr 6 (integerToNat $ cast len) recv
                       rest = substr (integerToNat $ cast $ 6 + len) (length recv) recv in
                       splitMessages rest (snoc acc msg)
       Nothing => acc

runFind : (installDir : String) -> (searchStr : String) -> IO (List String)
runFind installDir searchStr = do
  let cmd = "fd -p " ++ searchStr ++ " . " ++ installDir
  Right f <- popen cmd Read
    | Left err => do putStrLn (show err)
                     exitFailure
  ret <- rec f []
  pclose f
  pure ret
where
  rec : File -> List String -> IO (List String)
  rec f got = do Right line <- fGetLine f
                   | Left err => do putStrLn (show err)
                                    exitFailure
                 if line == "" || line == "\n"
                   then
                     pure $ reverse got
                   else do
                     let line = trimNewLineAtEnd line
                     nvimCommand $ "echom 'runFind: " ++ show line ++ "'"
                     rec f (line :: got)

record NamePositionInfo where
  constructor MkNamePositionInfo
  name : String
  filename : String
  startRow : Integer
  startColumn : Integer
  endRow : Integer
  endColumn : Integer

||| Checks if the provided SExp is a valid entry of a name-at output.
||| Parses the entry into a record with info in that case.
validateNameAtEntry : SExp -> Maybe NamePositionInfo
validateNameAtEntry (SExpList [ StringAtom name
                                    , StringAtom filename
                                    , IntegerAtom sr
                                    , IntegerAtom sc
                                    , IntegerAtom er
                                    , IntegerAtom ec]) =
  Just $ MkNamePositionInfo name filename sr sc er ec
validateNameAtEntry _ = Nothing

||| Opens the file in the focused buffer, navigating to the provided row and column
jumpToDef : (filePath : String) -> (row : Integer) -> (col : Integer) -> IO ()
jumpToDef path row col = do
  nvimCommand "write"
  nvimCommand $ "edit " ++ path
  nvimCommand $ "call setpos('.', [0, " ++ show (row + 1) ++ ", " ++ show (col + 1) ++ ", 0])"
  nvimCommand $ "normal! zz"

zipWithIndex : List a -> List (a, Nat)
zipWithIndex xs = zip xs [0 .. length xs `minus` 1]

process : (params : Parameters)
       => IDEResult
       -> IO ()
process (OK idx res) = do
  case !(primIO $ getCmdFromHistory idx) of
    Just (Interpret sel) => do
      let SExpList ((StringAtom ls) :: _) = res
        | x => nvimCommand $ "echom 'Invalid response to Interpret" ++ show res ++ "'"
      writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (LoadFile path _) => do
      writeToBuffer True $ !(primIO currenttime) ++ "\nSuccesfully reloaded " ++ path
      primIO $ deleteCmdInHistory idx
    Just (TypeOf name _) => do

      let SExpList ((StringAtom ls) :: _) = res
        | x => nvimCommand $ "echom 'Invalid response to TypeOf" ++ show res ++ "'"
      writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (NameAt name _) => do
      let SExpList [SExpList list] = res
        | x => nvimCommand $ "echom 'Invalid response to NameAt" ++ show res ++ "'"
      let Just validated@(first :: rest) = sequence $ map validateNameAtEntry list
        | Just [] => writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n"
                         ++ "Could not find anything on " ++ name
        | Nothing => nvimCommand $ "echom 'Invalid response to NameAt" ++ show res ++ "'"
      case rest of
        [] => do -- exactly one name return by the IDE
          [fullPath] <- runFind params.sourceDir first.filename -- one candidate found
            | multiple@(_ :: _) => do
                let relative = filter isRelative multiple
                case relative of
                  [] => nvimCommand "echoe \"Can't handle multiple absolute candidates\""
                  [singlePath] =>
                    jumpToDef singlePath first.startRow first.startColumn
                  _ => nvimCommand "echoe \"Can't handle multiple relative candidates\""
            | [] => nvimCommand "echoe 'Candidates not found'"
          jumpToDef fullPath first.startRow first.startColumn
        _ :: _ => do
          let fzfInput = map (\(x, i) => "[" ++ show i ++ "] " ++ x.filename)
                             (zipWithIndex validated)
          -- TODO previews
          fzfWindowBottom name fzfInput \file =>
            -- I (Russoul) don't know why, but the fzf vim plugin calls this callback with the empty string
            -- in addition to a normal call with the supplied choice.
            -- So we have to filter out that empty redundant call here
            when (file /= "") $ do
              nvimCommand $ "echom 'file is: " ++ file ++ "'"
              let _ :: iStr :: _ = matchlist file "\\v\\[([0-9]+)\\].+"
                | _ => nvimCommand $ "echoe 'Bad match while processing name-at'"
              let Just i = parsePositive {a = Nat} iStr
                | _ => nvimCommand $ "echoe 'Bad match while processing name-at'"
              let Just (entry, _) = find (\(_, j) => i == j) (zipWithIndex validated)
                | _ => nvimCommand $ "echoe 'Bad match while processing name-at'"
              nvimCommand $ "echom 'entry filename is: " ++ entry.filename ++ "'"
              candidates <- runFind params.sourceDir entry.filename
              case candidates of
                [] => writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n"
                                 ++ "Could not find anything on " ++ name
                [singlePath] => jumpToDef singlePath entry.startRow entry.startColumn
                many => do
                  -- Prioritise relative paths over absolute ones,
                  -- in case multiple files are found by the find utility
                  let relative = filter isRelative many
                  case relative of
                    [] => nvimCommand "echoe \"Can't handle multiple absolute candidates\""
                    [singlePath] =>
                      jumpToDef singlePath entry.startRow entry.startColumn
                    _ => nvimCommand "echoe \"Can't handle multiple relative candidates\""
              primIO $ deleteCmdInHistory idx
    Just (DocsFor name _) => do
      let SExpList ((StringAtom ls) :: _) = res
        | x => nvimCommand $ "echom 'Invalid response to DocsFor" ++ show res ++ "'"
      writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (CaseSplit line col name) => do
      let SExpList [StringAtom ls] = res
        | x => nvimCommand $ "echom 'Invalid response to CaseSplit" ++ show res ++ "'"
      primIO $ deleteLine (cast line)
      primIO $ appendLines False False (cast line - 1) ls
      primIO $ deleteCmdInHistory idx
    Just (AddClause line name) => do
      let SExpList [StringAtom ls] = res
        | x => nvimCommand $ "echom 'Invalid response to AddClause" ++ show res ++ "'"
      primIO $ appendLines False False (cast line) ls
      primIO $ deleteCmdInHistory idx
    Just (ExprSearch line name _ _) => do
      let SExpList [StringAtom ls] = res
        | x => nvimCommand $ "echom 'Invalid response to ExprSearch" ++ show res ++ "'"
      nvimCommand $ "s/" ++ (strCons '?' name) ++ "/" ++ ls ++ "/"
      primIO $ putLastSearch ls
      primIO $ deleteCmdInHistory idx
    Just ExprSearchNext => do
      -- TODO: How we rollback?
      let SExpList [StringAtom ls] = res
        | x => nvimCommand $ "echom 'Invalid response to ExprSearch" ++ show res ++ "'"
      writeToBuffer True $ "Inline substitution of next expression search is not available\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (GenerateDef line name) => do
      let SExpList [StringAtom ls] = res
        | x => nvimCommand $ "echom 'Invalid response to GenerateDef" ++ show res ++ "'"
      primIO $ appendLines False False (cast line) ls
      primIO $ deleteCmdInHistory idx
    Just GenerateDefNext => do
      -- TODO: How we rollback?
      let SExpList [StringAtom ls] = res
        | x => nvimCommand $ "echom 'Invalid response to ExprSearch" ++ show res ++ "'"
      writeToBuffer True $ "Inline substitution of next definition is not available\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (MakeLemma line name) => do
      -- TODO: How we find where to put the lemma? (Maybe, go to the previous paragraph and append there)
      let SExpList [StringAtom ls] = res
        | x => nvimCommand $ "echom 'Invalid response to ExprSearch" ++ show res ++ "'"
      writeToBuffer False $ "Inline substitution of make lemma is not available\n" ++ ls
      primIO $ deleteCmdInHistory idx
    Just (MakeCase line name) => do
      -- TODO: Feels like a hack
      let SExpList [StringAtom ls] = res
        | x => nvimCommand $ "echom 'Invalid response to MakeCase" ++ show res ++ "'"
      primIO $ deleteLine (cast line)
      primIO $ appendLines False False (cast line - 1) ls
      primIO $ deleteCmdInHistory idx
    Just (MakeWith line name) => do
      -- TODO: Feels like a hack
      let SExpList [StringAtom ls] = res
        | x => nvimCommand $ "echom 'Invalid response to MakeWith" ++ show res ++ "'"
      primIO $ deleteLine (cast line)
      primIO $ appendLines False False (cast line - 1) ls
      primIO $ deleteCmdInHistory idx
    Just (Metavariables _) => do
      -- let SExpList [StringAtom ls] = res
      --   | x => nvimCommand $ "echom 'Invalid response to Metavariables" ++ show res ++ "'"
      writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ (show res)
      primIO $ deleteCmdInHistory idx
    Just (BrowseNamespace name) => do
      let SExpList ((StringAtom ls) :: _) = res
        | x => nvimCommand $ "echom 'Invalid response to BrowseNamespace" ++ show res ++ "'"
      writeToBuffer False $ "-- " ++ !(primIO currenttime) ++ "\n" ++ ls
      primIO $ deleteCmdInHistory idx
--     Just (EnableSyntax _) => do
--       let SExpList ((StringAtom ls) :: _) = res
--         | x => nvimCommand $ "echom 'Invalid response to EnableSyntax"++ show res ++ "'"
--       writeToBuffer True $ !(primIO currenttime) ++ "\nServer message: \"" ++ ls ++ "\""
--       primIO $ deleteCmdInHistory idx
    Just GetOptions => do
      writeToBuffer True (show res)
      primIO $ deleteCmdInHistory idx
    x => pure ()
process (Warning idx (SExpList [StringAtom file, start, end, StringAtom msg, _])) = pure () -- TODO: disabled until fix in compiler
process (Error idx (SExpList ((StringAtom ls) :: _))) = do
  writeToBuffer False $ "-- Error " ++ !(primIO currenttime) ++ "\n" ++ ls
  primIO $ deleteCmdInHistory idx
process (WriteString idx res) = do
  let msg = !(primIO currenttime) ++ "\nServer message: \"" ++ res ++ "\""
  writeToBuffer True msg
  primIO $ deleteCmdInHistory idx
process (Output idx res) = primIO $ deleteCmdInHistory idx
process (Version (SExpList [IntegerAtom major, IntegerAtom minor])) = do
  let msg = !(primIO currenttime) ++ "\nIdris2 IDE Mode server version " ++ show major ++ "." ++ show minor
  writeToBuffer True msg
process _ = nvimCommand $ "echom 'Invalid message from the server'"

connectIdris2 : (params : Parameters)
             => String
             -> Int
             -> IO OpaqueDict
connectIdris2 host port = do
  nvimCommand "echom 'Starting connection...'"
  client <- newTCP
  nvimCommand "echom 'Created new tcp socket...'"
  connect client host port
    (do readStart client
          (\recv => do
              let msgs = splitMessages recv []
              for_ msgs $ \msg => do
                let head = substr 0 5 msg
                if (head == "(:out")
                   then nvimCommand $ "echo 'skipped output'"
                   else do let Right sexp = parseSExp msg
                             | Left err => nvimCommand $ "echom 'invalid response: " ++ show err ++ "'"
                           let Just res = getResult sexp
                             | Nothing => nvimCommand $ "echom 'invalid response: " ++ show sexp ++ "'"
                           process res)
          (\err => nvimCommand $ "echom 'read err: " ++ err ++ "'")
          (pure ()))
    (\err => nvimCommand $ "echom 'connect error: " ++ err ++ "'")
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
                     => (params : Parameters)
                     => Ref Server (Maybe OpaqueDict)
                     => Ref Client (Maybe OpaqueDict)
                     => (String, Int)
                     -> (String, Int)
                     -> io ()
spawnAndConnectIdris2 (shost, sport) (chost, cport) = do
  Nothing <- get Server
    | Just _ => nvimCommand "echom 'Idris2 process already spawned'"
  stdout <- newPipe
  let opts = spawnOpts (shost ++ ":" ++ show sport) stdout
  d <- spawn "idris2" opts (\code, signal => nvimCommand $ "echom 'Idris2 process closed with code " ++ show code ++ "'")
  let handle : OpaqueDict = getField d "handle"
  put Server (Just handle)
  readStart stdout (\msg => do nvimCommand $ "echom 'server: " ++ msg ++ "'"
                               if trim msg == show sport
                                  then do client <- connectIdris2 chost cport
                                          primIO $ setGlobalClient client -- TODO: can we remove it?
                                          put Client (Just client)
                                          readStop stdout
                                  else do nvimCommand $ "echom 'wrong port " ++ msg ++ "'"
                                          quitServer)
                   (\err => do nvimCommand $ "echom 'err: " ++ err ++ "'"
                               quitServer)
                   (do nvimCommand "echom 'Idris2 stdout closed'"
                       quitServer)

main : IO ()
main = do
  loadCommands

  serverRef <- newRef Server $ the (Maybe OpaqueDict) Nothing
  clientRef <- newRef Client $ the (Maybe OpaqueDict) Nothing

  sourceDir <- getSourceDirectory

  autostart <- getGlobalBoolVar "idris2_autostart" True

  let params = MkParameters sourceDir

  when autostart $ do
    nvimCommand "set maxfuncdepth=10000"
    nvimCommand "echom 'starting idris2 ide mode plugin'"

    externalClientOpt <- getGlobalBoolVar "idris2_external_server" False
    if externalClientOpt
       then do nvimCommand "echom 'starting with external server'"
               host <- getGlobalStringVar "idris2_external_host" "127.0.0.1"
               port <- getGlobalIntVar "idris2_external_port" 38398
               client <- connectIdris2 host port
               primIO $ setGlobalClient client
       else do nvimCommand "echom 'starting server'"
               port <- getGlobalIntVar "idris2_server_port" 38398
               spawnAndConnectIdris2 ("localhost", port) ("127.0.0.1", port)

    -- KEYBINDINGS
    nnoremap !(getGlobalStringVar "idris2_loadCurrent_key" "<LocalLeader>r")
              (commandBinding True `{{loadCurrent}})
    nnoremap !(getGlobalStringVar "idris2_typeOf_key" "<LocalLeader>t")
              (commandBinding True `{{typeOf}})
    nnoremap !(getGlobalStringVar "idris2_typeOfPrompt_key" "<LocalLeader>T")
              (commandBinding True `{{typeOfPrompt}})
    vnoremap !(getGlobalStringVar "idris2_typeOf_key" "<LocalLeader>t")
              (commandBinding True `{{typeOfSel}})
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
    nnoremap !(getGlobalStringVar "idris2_jumpTo_key" "<LocalLeader>j")
              (commandBinding True `{{nameAt}})
    nnoremap !(getGlobalStringVar "idris2_jumpToPrompt_key" "<LocalLeader>J")
              (commandBinding True `{{nameAtPrompt}})
    vnoremap !(getGlobalStringVar "idris2_jumpTo_key" "<LocalLeader>j")
              (commandBinding True `{{nameAtSel}})
    vnoremap !(getGlobalStringVar "idris2_interpret_key" "<LocalLeader>e")
              (commandBinding True `{{interpret}}) -- evaluates selected code

    -- AUTOCOMMANDS
    loadOnStart <- getGlobalBoolVar "idris2_load_on_start" True
    when loadOnStart $ do
      nvimCommand "augroup IdrisLoadOnRead"
      nvimCommand " autocmd!"
      nvimCommand $ "autocmd BufNewFile,BufRead *.idr " ++ commandBinding False `{{loadCurrent}}
      nvimCommand "augroup end"
    -- Set up the response buffer
    nvimCommand "augroup IdrisSetBufType"
    nvimCommand $ " autocmd!"
    nvimCommand $ " autocmd BufEnter " ++ responseBufferName ++ " set buftype=nofile|set syntax=idris2"
    nvimCommand "augroup end"

    -- RESPONSE BUFFER
    nvimCommand "vertical rightbelow split"
    nvimCommand "badd idris-response"
    nvimCommand "b idris-response"
    nvimCommand "set buftype=nofile"
    nvimCommand "wincmd h"
