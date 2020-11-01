module Commands

import Data.Strings
import Language.Reflection

import public Idris.IDEMode.Commands
import Utils.Hex

import Foreign

%language ElabReflection

data IDEResult
  = OK SExp
  | Warning SExp
  | Error SExp
  | WriteString SExp
  | Output SExp
  | Version SExp

export
Show IDEResult where
  show (OK x) = "OK: " ++ show x
  show (Warning x) = "Warning: " ++ show x
  show (Error x) = "Error: " ++ show x
  show (WriteString x) = "WriteString: " ++ show x
  show (Output x) = "Output: " ++ show x
  show (Version x) = "Version: " ++ show x

export
getResult : SExp -> Maybe IDEResult
getResult (SExpList [SymbolAtom "return", SExpList (SymbolAtom "ok" :: xs), IntegerAtom _]) = Just $ OK $ SExpList xs
getResult (SExpList [SymbolAtom "return", SExpList (SymbolAtom "warning" :: xs), IntegerAtom _]) = Just $ Warning $ SExpList xs
getResult (SExpList [SymbolAtom "return", SExpList (SymbolAtom "error" :: xs), IntegerAtom _]) = Just $ Error $ SExpList xs
getResult (SExpList (SymbolAtom "write-string" :: xs)) = Just $ WriteString $ SExpList xs
getResult (SExpList (SymbolAtom "output" :: xs)) = Just $ Output $ SExpList xs
getResult (SExpList (SymbolAtom "protocol-version" :: xs)) = Just $ Version $ SExpList xs
getResult _ = Nothing

export
buildCommand : IDECommand -> String
buildCommand cmd =
  let s = show $ SExpList [toSExp cmd, IntegerAtom 1] in
      leftPad '0' 6 (asHex (cast (length s))) ++ s

extractName : String -> String
extractName name = case strM name of
                        StrCons '?' name' => name'
                        _ => name

export
loadCurrent : IO ()
loadCurrent = do
  client <- primIO getGlobalClient
  path <- filePath
  write client (buildCommand $ LoadFile path Nothing)

export
typeOf : IO ()
typeOf = do
  client <- primIO getGlobalClient
  name <- extractName <$> cursorWord
  write client (buildCommand $ TypeOf name Nothing)

export
docOverview : IO ()
docOverview = do
  client <- primIO getGlobalClient
  name <- extractName <$> cursorWord
  write client (buildCommand $ DocsFor name (Just Overview))

export
docFull : IO ()
docFull = do
  client <- primIO getGlobalClient
  name <- extractName <$> cursorWord
  write client (buildCommand $ DocsFor name (Just Full))

export
caseSplit : IO ()
caseSplit = do
  client <- primIO getGlobalClient
  line <- cast <$> cursorLine
  col <- cast <$> cursorColumn
  name <- extractName <$> cursorWord
  -- TODO: replace in code
  write client (buildCommand $ CaseSplit line col name)

export
addClause : IO ()
addClause = do
  client <- primIO getGlobalClient
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  -- TODO: replace in code
  write client (buildCommand $ AddClause line name)

-- NOT IMPLEMENTED YET: addMissing : IO ()

export
exprSearch : IO ()
exprSearch = do
  client <- primIO getGlobalClient
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  -- TODO: add hints option
  write client (buildCommand $ ExprSearch line name [] False)

export
exprSearchNext : IO ()
exprSearchNext = do
  client <- primIO getGlobalClient
  -- TODO: add hints option
  -- TODO: replace in code
  write client (buildCommand ExprSearchNext)

export
generateDef : IO ()
generateDef = do
  client <- primIO getGlobalClient
  -- TODO: replace in code
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  write client (buildCommand $ GenerateDef line name)

export
generateDefNext : IO ()
generateDefNext = do
  client <- primIO getGlobalClient
  -- TODO: replace in code
  write client (buildCommand GenerateDefNext)

export
makeLemma : IO ()
makeLemma = do
  client <- primIO getGlobalClient
  -- TODO: replace in code
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  write client (buildCommand $ MakeLemma line name)

export
makeCase : IO ()
makeCase = do
  client <- primIO getGlobalClient
  -- TODO: replace in code
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  write client (buildCommand $ MakeCase line name)

export
makeWith : IO ()
makeWith = do
  client <- primIO getGlobalClient
  -- TODO: replace in code
  line <- cast <$> cursorLine
  name <- extractName <$> cursorWord
  write client (buildCommand $ MakeWith line name)

-- NOT IMPLEMENTED YET: directive : IO ()
-- NOT IMPLEMENTED YET: apropos : IO ()

export
metavariables : IO ()
metavariables = do
  client <- primIO getGlobalClient
  write client (buildCommand $ Metavariables 1)

-- NOT IMPLEMENTED YET: whocalls : IO ()
-- NOT IMPLEMENTED YET: callswho : IO ()

export
browseNamespace : IO ()
browseNamespace = do
  client <- primIO getGlobalClient
  name <- extractName <$> cursorWord'
  write client (buildCommand $ BrowseNamespace name)

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
  write client (buildCommand $ EnableSyntax b)

export
getOptions : IO ()
getOptions = do
  client <- primIO getGlobalClient
  write client (buildCommand GetOptions)

export
loadCommands : IO ()
loadCommands = do
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
commandBinding : Name -> Elab String
commandBinding n = do
  [(name, _)] <- getType n
    | _ => fail $ show n ++ " is not unique in scope"
  pure $ ":lua idris['" ++ show name ++ "']('%World')<CR>"
