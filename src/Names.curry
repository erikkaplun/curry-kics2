--- --------------------------------------------------------------------------
--- This module contains operations to change names of the original program
--- into names used in the target program and vice versa.
--- 
--- @author Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version November 2012
--- --------------------------------------------------------------------------
module Names where

import Char            (isAlphaNum)
import FilePath        ((</>))
import List            (intersperse)

import AbstractHaskell (QName)
import Classification  (NDClass (..), ConsHOClass (..), FuncHOClass (..))
import Files           (withComponents, (</?>))

-- ---------------------------------------------------------------------------
-- Renaming file names
-- ---------------------------------------------------------------------------

renameFile :: Bool -> String -> String
renameFile trace
  | trace     = addTrace . renameModule
  | otherwise = renameModule -- until hierarchical module names are supported

externalFile :: String -> String
externalFile = withComponents id ("External_" ++) (const "hs")

destFile :: Bool -> String -> String -> String
destFile trace subdir = withComponents (</?> subdir) (renameFile trace) (const "hs")

analysisFile :: String -> String -> String
analysisFile subdir = withComponents (</?> subdir) (renameFile False) (const "nda")

--- Auxiliary file containing some basic information about functions
--- (might become unnecessary in the future)
funcInfoFile :: String -> String -> String
funcInfoFile subdir = withComponents (</?> subdir) (renameFile False) (const "info")

-- ---------------------------------------------------------------------------
-- Renaming modules
-- ---------------------------------------------------------------------------

prelude :: String
prelude = "Prelude"

basics :: String
basics = "Basics"

curryPrelude :: String
curryPrelude = renameModule prelude

renameModule :: String -> String
renameModule = ("Curry_" ++)

addTrace :: String -> String
addTrace = renameModule . ("Trace_" ++) . unRenameModule

removeTrace :: String -> String
removeTrace = renameModule . dropPrefix "Trace_" . unRenameModule

unRenameModule :: String -> String
unRenameModule = dropPrefix "Curry_"

isCurryModule :: String -> Bool
isCurryModule m = take 6 m == "Curry_"

isHaskellModule :: String -> Bool
isHaskellModule = not . isCurryModule

-- ---------------------------------------------------------------------------
-- Building constructors
-- ---------------------------------------------------------------------------

mkChoiceName :: QName -> QName
mkChoiceName = withQName id ("Choice_" ++)

mkChoicesName :: QName -> QName
mkChoicesName = withQName id ("Choices_" ++)

mkFailName :: QName -> QName
mkFailName = withQName id ("Fail_" ++)

mkGuardName :: QName -> QName
mkGuardName = withQName id ("Guard_" ++)

mkFoConsName :: QName -> QName
mkFoConsName = id

mkHoConsName :: QName -> QName
mkHoConsName = withQName id ("HO_" ++)

-- ---------------------------------------------------------------------------
-- (non)determinism prefixes
-- ---------------------------------------------------------------------------

unRenamePrefixedFunc :: QName -> QName
unRenamePrefixedFunc = unRenameQName . dropFuncPrefix

dropFuncPrefix :: QName -> QName
dropFuncPrefix (m, f) = case f of
  'd' : '_' : fun       -> (m, fun)
  'n' : 'd' : '_' : fun -> (m, fun)
  _                     -> (m, f)

-- Compute the determinism prefix of a curry function
-- 1st arg: is the function used for compilation in determinism mode
-- 2nd arg: determinism classification of the function
-- 3rd arg: higher-order classification of the function
funcPrefix :: Bool -> NDClass -> FuncHOClass -> String
funcPrefix _     D  FuncFO        = "d_"
funcPrefix _     D  (FuncHORes _) = "d_"
funcPrefix True  D  FuncHO        = "d_"  -- "dho_"
funcPrefix False D  FuncHO        = "nd_" -- "ndho_"
funcPrefix _     ND _             = "nd_"

-- Compute the determinism prefix of a Curry constructor
-- 1st arg: is the function used for compilation in determinism mode
-- 2nd arg: classification of the constructor
consPrefix :: Bool -> ConsHOClass -> String
consPrefix _     ConsFO        = ""
consPrefix True  ConsHO        = ""
consPrefix False ConsHO        = "HO_"

-- ---------------------------------------------------------------------------
-- General renaming of functions and constructors
-- ---------------------------------------------------------------------------

renameQNameTrace :: QName -> QName
renameQNameTrace qn = if isCurryModule (fst qn) then withQName addTrace id qn
                                                else qn

renameQName :: QName -> QName
renameQName = withQName renameModule genRename

unRenameQName :: QName -> QName
unRenameQName = withQName unRenameModule unGenRename

externalFunc :: QName -> QName
externalFunc = withQName id ("external_" ++)

fromExternalFunc :: QName -> QName
fromExternalFunc = withQName id (dropPrefix "external_")

mkGlobalName :: QName -> QName
mkGlobalName = withQName id ("global_" ++)

fromGlobalName :: QName -> QName
fromGlobalName = withQName id (dropPrefix "global_")

genRename :: String -> String
genRename n
  | n == "[]"     = "OP_List"
  | n == ":"      = "OP_Cons"
  | n == "()"     = "OP_Unit"
  | head n == '(' = "OP_Tuple" ++ show (length n - 1)
  | otherwise     = replaceNonIdChars "C_" "OP_" n

unGenRename :: String -> String
unGenRename n
  | n == "OP_List"         = "[]"
  | n == "OP_Cons"         = ":"
  | n == "OP_Unit"         = "()"
  | take 8 n == "OP_Tuple" = mkTuple $ parseNat $ drop 8 n
  | take 2 n == "C_"       = drop 2 n
  | take 3 n == "OP_"      = unRenameOp  (drop 3 n)
  | take 3 n == "HO_"      = unGenRename (drop 3 n)
  | otherwise              = n

mkTuple :: Int -> String
mkTuple n = '(' : replicate (n - 1) ',' ++ ")"

parseNat :: String -> Int
parseNat xs = parse' 0 xs
  where
  parse' s []     = s
  parse' s (c:cs) = parse' (10 * s + ord c - ord '0') cs

-- ---------------------------------------------------------------------------
-- Auxiliaries
-- ---------------------------------------------------------------------------

withQName :: (String -> String) -> (String -> String) -> QName -> QName
withQName fq fn (q, n) = (fq q, fn n)

dropPrefix :: String -> String -> String
dropPrefix pfx s
  | take n s == pfx = drop n s
  | otherwise       = s
  where n = length pfx

isInfixName :: String -> Bool
isInfixName = all (`elem` "?!#$%^&*+=-<>.:/\\|")

opRenaming :: [(Char, String)]
opRenaming =
  [ ('_' , "uscore"   )
  , ('~' , "tilde"    )
  , ('!' , "bang"     )
  , ('@' , "at"       )
  , ('#' , "hash"     )
  , ('$' , "dollar"   )
  , ('%' , "percent"  )
  , ('^' , "caret"    )
  , ('&' , "amp"      )
  , ('*' , "star"     )
  , ('+' , "plus"     )
  , ('-' , "minus"    )
  , ('=' , "eq"       )
  , ('<' , "lt"       )
  , ('>' , "gt"       )
  , ('?' , "qmark"    )
  , ('.' , "dot"      )
  , ('/' , "slash"    )
  , ('|' , "bar"      )
  , ('\\', "backslash")
  , (':' , "colon"    )
  , ('(' , "lparen"   )
  , (')' , "rparen"   )
  , ('[' , "lbracket" )
  , (']' , "rbracket" )
  , (',' , "comma"    )
  , ('\'', "tick"     )
  ]

unRenameOp :: String -> String
unRenameOp = concatMap toOpChar . splitAll (== '_')
  where toOpChar s = case lookup s (map swap opRenaming) of
          Just c  -> [c]
          Nothing -> s
        swap (a, b) = (b, a)

splitAll :: (a -> Bool) -> [a] -> [[a]]
splitAll _ []       = []
splitAll p xs@(_:_) = f : splitAll p (drop 1 s)
  where (f, s) = break p xs

-- |Replaces characters that are not allowed in haskell identifiers.
-- If no characters were replaced, the first prefix is used,
-- otherwise the second one.
replaceNonIdChars :: String -> String -> String -> String
replaceNonIdChars pfxNonOp pfxOp str = case strs of
  [] -> error "Names.replaceNonIdChars: empty identifier"
  _  -> if and (map (all isIdentChar) strs)
          then pfxNonOp ++ concat strs
          else pfxOp    ++ intercalate "_" (map (concatMap showOpChar) strs)
 where  strs = spanAll isAlphaNum str

        intercalate xs xss = concat (intersperse xs xss)

        showOpChar  c = case lookup c opRenaming of
          Just ren -> ren
          Nothing  -> [c]

        isIdentChar c = isAlphaNum c || c == '_' || c == '\''

spanAll :: (a -> Bool) -> [a] -> [[a]]
spanAll p xs = (if null pfx then id else (pfx:)) $ case rest of
  []     -> []
  (x:ys) -> [x] : spanAll p ys
 where (pfx, rest) = span p xs
