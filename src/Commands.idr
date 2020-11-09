module Commands

import Data.Strings
import Language.Reflection

import public Idris.IDEMode.Commands
import Utils.Hex

import Foreign

%language ElabReflection

public export
data IDEResult
  = OK Int SExp
  | Warning Int SExp
  | Error Int SExp
  | WriteString Int String
  | Output Int SExp
  | Version SExp

export
Show IDEResult where
  show (OK i x) = show i ++ " OK: " ++ show x
  show (Warning i x) = show i ++ " Warning: " ++ show x
  show (Error i x) = show i ++ " Error: " ++ show x
  show (WriteString i x) = show i ++ " WriteString: " ++ show x
  show (Output i x) = show i ++ " Output: " ++ show x
  show (Version x) = "Version: " ++ show x

export
Show IDECommand where
  showPrec p (Interpret x) = showCon p "Interpret" $ showArg x
  showPrec p (LoadFile x y) = showCon p "LoadFile" $ showArg x ++ showArg y
  showPrec p (TypeOf x y) = showCon p "TypeOf" $ showArg x ++ showArg y
  showPrec p (CaseSplit x y z) = showCon p "CaseSplit" $ showArg x ++ showArg y ++ showArg z
  showPrec p (AddClause x y) = showCon p "AddClause" $ showArg x ++ showArg y
  showPrec p (AddMissing x y) = showCon p "AddMissing" $ showArg x ++ showArg y
  showPrec p (ExprSearch x y xs z) = showCon p "ExprSearch" $ showArg x ++ showArg y ++ showArg xs ++ showArg z
  showPrec p ExprSearchNext = "ExprSearchNext"
  showPrec p (GenerateDef x y) = showCon p "GenerateDef" $ showArg x ++ showArg y
  showPrec p GenerateDefNext = "GenerateDefNext"
  showPrec p (MakeLemma x y) = showCon p "MakeLemma" $ showArg x ++ showArg y
  showPrec p (MakeCase x y) = showCon p "MakeCase" $ showArg x ++ showArg y
  showPrec p (MakeWith x y) = showCon p "MakeWith" $ showArg x ++ showArg y
  showPrec p (DocsFor x y) = showCon p "DocsFor" $ showArg x
  showPrec p (Directive x) = showCon p "Directive" $ showArg x
  showPrec p (Apropos x) = showCon p "Apropos" $ showArg x
  showPrec p (Metavariables x) = showCon p "Metavariables" $ showArg x
  showPrec p (WhoCalls x) = showCon p "WhoCalls" $ showArg x
  showPrec p (CallsWho x) = showCon p "CallsWho" $ showArg x
  showPrec p (BrowseNamespace x) = showCon p "BrowseNamespace" $ showArg x
  showPrec p (NormaliseTerm x) = showCon p "NormaliseTerm" $ showArg x
  showPrec p (ShowTermImplicits x) = showCon p "ShowTermImplicits" $ showArg x
  showPrec p (HideTermImplicits x) = showCon p "HideTermImplicits" $ showArg x
  showPrec p (ElaborateTerm x) = showCon p "ElaborateTerm" $ showArg x
  showPrec p (PrintDefinition x) = showCon p "PrintDefinition" $ showArg x
  showPrec p (ReplCompletions x) = showCon p "ReplCompletions" $ showArg x
  showPrec p (EnableSyntax x) = showCon p "EnableSyntax" $ showArg x
  showPrec p Version = "Version"
  showPrec p GetOptions = "GetOptions"

export
getResult : SExp -> Maybe IDEResult
getResult (SExpList [SymbolAtom "return", SExpList (SymbolAtom "ok" :: xs), IntegerAtom idx]) =
  Just $ OK (cast idx) (SExpList xs)
getResult (SExpList [SymbolAtom "return", SExpList (SymbolAtom "warning" :: xs), IntegerAtom idx]) =
  Just $ Warning (cast idx) (SExpList xs)
getResult (SExpList [SymbolAtom "warning", xs, IntegerAtom idx]) =
  Just $ Warning (cast idx) xs
getResult (SExpList [SymbolAtom "return", SExpList (SymbolAtom "error" :: xs), IntegerAtom idx]) =
  Just $ Error (cast idx) (SExpList xs)
getResult (SExpList [SymbolAtom "write-string", StringAtom xs, IntegerAtom idx]) =
  Just $ WriteString (cast idx) xs
getResult (SExpList [SymbolAtom "output", xs, IntegerAtom idx]) =
  Just $ Output (cast idx) xs
getResult (SExpList (SymbolAtom "protocol-version" :: xs)) =
  Just $ Version (SExpList xs)
getResult _ = Nothing

export
buildCommand : IDECommand -> IO String
buildCommand cmd = do
  idx <- primIO genHistoryIndex
  primIO $ putCmdInHistory idx cmd
  let s = show $ SExpList [toSExp cmd, IntegerAtom (cast idx)]
  pure $ leftPad '0' 6 (asHex $ cast $ length s) ++ s

extractName : String -> String
extractName name = case strM name of
                        StrCons '?' name' => name'
                        _ => name
export
interpret : IO ()
interpret = do
  client <- primIO getGlobalClient
  sel <- getSelection
  write client !(buildCommand $ Interpret sel)

export
loadCurrent : IO ()
loadCurrent = do
  saveBuffer
  client <- primIO getGlobalClient
  path <- filePath
  write client !(buildCommand $ LoadFile path Nothing)

export
typeOf : IO ()
typeOf = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  name <- extractName <$> cursorWord
  write client !(buildCommand $ TypeOf name Nothing)

export
docOverview : IO ()
docOverview = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  name <- extractName <$> cursorWord
  write client !(buildCommand $ DocsFor name (Just Overview))

export
docFull : IO ()
docFull = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  name <- extractName <$> cursorWord
  write client !(buildCommand $ DocsFor name (Just Full))

export
caseSplit : IO ()
caseSplit = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  line <- cast <$> cursorLine
  col <- cast <$> cursorColumn
  name <- extractName <$> cursorWord
  write client !(buildCommand $ CaseSplit line col name)

export
addClause : IO ()
addClause = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  write client !(buildCommand $ AddClause line name)

-- NOT IMPLEMENTED YET: addMissing : IO ()

export
exprSearch : IO ()
exprSearch = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  -- TODO: add hints option
  write client !(buildCommand $ ExprSearch line name [] False)

export
exprSearchNext : IO ()
exprSearchNext = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  -- TODO: add hints option
  write client !(buildCommand ExprSearchNext)

export
generateDef : IO ()
generateDef = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  write client !(buildCommand $ GenerateDef line name)

export
generateDefNext : IO ()
generateDefNext = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  write client !(buildCommand GenerateDefNext)

export
makeLemma : IO ()
makeLemma = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  write client !(buildCommand $ MakeLemma line name)

export
makeCase : IO ()
makeCase = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  -- TODO: replace in code
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  write client !(buildCommand $ MakeCase line name)

export
makeWith : IO ()
makeWith = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  -- TODO: replace in code
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  write client !(buildCommand $ MakeWith line name)

-- NOT IMPLEMENTED YET: directive : IO ()
-- NOT IMPLEMENTED YET: apropos : IO ()

export
metavariables : IO ()
metavariables = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  write client !(buildCommand $ Metavariables 1)

-- NOT IMPLEMENTED YET: whocalls : IO ()
-- NOT IMPLEMENTED YET: callswho : IO ()

export
browseNamespace : IO ()
browseNamespace = do
  when !isBufferModified loadCurrent
  client <- primIO getGlobalClient
  name <- extractName <$> cursorWord'
  write client !(buildCommand $ BrowseNamespace name)

-- NOT IMPLEMENTED YET: normaliseTerm : IO ()
-- NOT IMPLEMENTED YET: showTermImplicits : IO ()
-- NOT IMPLEMENTED YET: hideTermImplicits : IO ()
-- NOT IMPLEMENTED YET: elaborateTerm : IO ()
-- NOT IMPLEMENTED YET: printDefinition : IO ()
-- NOT IMPLEMENTED YET: replCompletions : IO ()

export
enableSyntax : Bool -> IO ()
enableSyntax b = do
  client <- primIO getGlobalClient
  write client !(buildCommand $ EnableSyntax b)

export
getOptions : IO ()
getOptions = do
  client <- primIO getGlobalClient
  write client !(buildCommand GetOptions)

export
loadCommands : IO ()
loadCommands = do
  () <- if False then interpret else pure ()
  () <- if False then loadCurrent else pure ()
  () <- if False then typeOf else pure ()
  () <- if False then docOverview else pure ()
  () <- if False then docFull else pure ()
  () <- if False then caseSplit else pure ()
  () <- if False then addClause else pure ()
  () <- if False then exprSearch else pure ()
  () <- if False then exprSearchNext else pure ()
  () <- if False then generateDef else pure ()
  () <- if False then generateDefNext else pure ()
  () <- if False then makeLemma else pure ()
  () <- if False then makeCase else pure ()
  () <- if False then makeWith else pure ()
  () <- if False then metavariables else pure ()
  () <- if False then browseNamespace else pure ()
  () <- if False then enableSyntax True else pure ()
  () <- if False then getOptions else pure ()
  pure ()

export %macro
commandBinding : Bool -> Name -> Elab String
commandBinding b n = do
  [(name, _)] <- getType n
    | _ => fail $ show n ++ " is not unique in scope"
  if b
     then pure $ ":lua idris['" ++ show name ++ "'](W)<CR>"
     else pure $ "lua idris['" ++ show name ++ "'](W)"
