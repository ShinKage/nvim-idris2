module Commands

import public Idris.IDEMode.Commands
import Utils.Hex

import Foreign

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

export
unsafeLoadCurrent : IO ()
unsafeLoadCurrent = do
  client <- primIO getGlobalClient
  path <- filePath
  write client (buildCommand $ LoadFile path Nothing)

export
unsafeTypeOf : IO ()
unsafeTypeOf = do
  client <- primIO getGlobalClient
  name <- cursorWord
  write client (buildCommand $ TypeOf name Nothing)
