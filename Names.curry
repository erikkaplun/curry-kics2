-----------------------------------------------------------------------
--- Operations to change names of the original program into names
--- used in the target program
-----------------------------------------------------------------------
module Names where

import Char (isAlphaNum)
import List (intersperse)
import Maybe (fromJust, isJust)

import AbstractHaskell
import Base
import CompilerOpts
import Files

-- Name of the module providing external function definitions.
externalModule :: String
externalModule = "External"

-- Generating Names for introduced constructors
mkChoiceName  (q, n) = (q, "Choice"  +|+ n)
mkChoicesName (q, n) = (q, "Choices" +|+ n)
mkFailName    (q, n) = (q, "Fail"    +|+ n)
mkGuardName   (q, n) = (q, "Guard"   +|+ n)
mkFoConsName  (q, n) = (q, n)
mkHoConsName  (q, n) = (q, "HO" +|+ n)
s +|+ t = s ++ "_" ++ t

prelude :: String
prelude = "Prelude"

basics :: String
basics = "Basics"

curryPrelude :: String
curryPrelude = renameModule "Prelude"

renameModule :: String -> String
renameModule = ("Curry_" ++)

unRenameModule :: String -> String
unRenameModule n | take 6 n == "Curry_" = drop 6 n
                 | otherwise            = n

isCurryModule :: String -> Bool
isCurryModule m = take 6 m == "Curry_"

isHaskellModule :: String -> Bool
isHaskellModule = not . isCurryModule

renameQName :: QName -> QName
renameQName (q, n) = (renameModule q, genRename n)

genRename :: String -> String
genRename n
  | n == "[]"     = "OP_List"
  | n == ":"      = "OP_Cons"
  | n == "()"     = "OP_Unit"
  | head n == '(' = "OP_Tuple" ++ show (length n - 1)
  | otherwise     = replaceNonIdChars "C_" "OP_" n

-- TODO: Also for OP_Tuple ?
unGenRename :: String -> String
unGenRename n
  | n == "OP_List"   = "[]"
  | n == "OP_Cons"   = ":"
  | n == "OP_Unit"   = "()"
  | take 2 n == "C_" = drop 2 n
  | otherwise        = n

externalFunc :: QName -> QName
externalFunc (q, n) = (q, "external_" ++ n)

-- Compute the determinism prefix of a curry function
-- 1st arg: is the function used for compilation in determinism mode
-- 2nd arg: determinism classification of the function
-- 3rd arg: higher-order classification of the function
funcPrefix :: Bool -> NDClass -> HOClass -> String
funcPrefix _     D  FO = "d_"
funcPrefix True  D  HO = "d_"  -- "dho_"
funcPrefix False D  HO = "nd_" -- "ndho_"
funcPrefix _     ND _  = "nd_"

-- Compute the determinism prefix of a curry constructor
-- 1st arg: is the function used for compilation in determinism mode
-- 2nd arg: classification of the constructor
consPrefix :: Bool -> HOClass -> String
consPrefix _     FO = ""
consPrefix True  HO = ""
consPrefix False HO = "HO_"

-- rename modules
mkModName :: String -> String
mkModName = ("Curry_" ++)

renameFile :: String -> String
renameFile = renameModule -- until hierarchical module names are supported

destFile :: String-> String -> String
destFile subdir = withPath (</> subdir)        -- apply subdir
                . withExtension (const ".hs")  -- .hs extension
                . withBaseName renameFile      -- renaming

analysisFile :: String -> String -> String
analysisFile subdir = withPath (</> subdir) -- apply subdir
                    . withExtension (const ".nda")
                    . withBaseName renameFile

-- Auxiliary file containing some basic information about functions
-- (might become unnecessary in the future)
funcInfoFile :: String -> String -> String
funcInfoFile subdir = withPath (</> subdir) -- apply subdir
                    . withExtension (const ".info")
                    . withBaseName renameFile

isInfixName :: String -> Bool
isInfixName = all (`elem` "?!#$%^&*+=-<>.:/\\|")

showOpChar :: Char -> String
showOpChar c = case c of
  '_' -> "_" --"underscore" TODO: Can this lead to a name clash?
  '~' -> "tilde"
  '!' -> "bang"
  '@' -> "at"
  '#' -> "hash"
  '$' -> "dollar"
  '%' -> "percent"
  '^' -> "caret"
  '&' -> "ampersand"
  '*' -> "star"
  '+' -> "plus"
  '-' -> "minus"
  '=' -> "eq"
  '<' -> "lt"
  '>' -> "gt"
  '?' -> "qmark"
  '.' -> "dot"
  '/' -> "slash"
  '|' -> "bar"
  '\\' ->"backslash"
  ':' -> "colon"
  '(' -> "oparen"
  ')' -> "cparen"
  '[' -> "obracket"
  ']' -> "cbracket"
  ',' -> "comma"
  '\'' -> "tick"
  _   -> error $ "unexpected symbol: " ++ show c

-- | replaces characters that are not valid haskell identifiers,
-- | if there were no characters replaced, the first prefix,
-- | otherwise the snd prefix ist prepended
replaceNonIdChars :: String -> String -> String -> String
replaceNonIdChars pfxNonOp pfxOp str = case strings of
  []  -> error "replaceNonIdChars: empty identifier"
  [s] -> if isAlphaNum (head str)
            then pfxNonOp ++ s
            else pfxOp    ++ s
  _   -> pfxOp ++ concat (intersperse "_" strings)
 where strings       = separateAndReplace isIdentChar showOpChar str
       isIdentChar c = isAlphaNum c || c == '_' || c == '\''

separateAndReplace :: (a -> Bool) -> (a -> [a]) -> [a] -> [[a]]
separateAndReplace pred f list = case rest of
  [] -> case sep of
    [] -> []
    _  -> [sep]
  (x:xs) -> case sep of
    [] -> f x : separateAndReplace pred f xs
    _  -> sep : f x : separateAndReplace pred f xs
 where (sep,rest) = break  (not . pred) list

-- rename type constructors
{-
mkTypeName :: String -> String
mkTypeName n
  | n == "[]"     = "OP_List"
  | n == "()"     = "OP_Unit"
  | head n == '(' = "OP_Tuple" ++ show (length n - 1)
  | otherwise     = replaceNonIdChars "C_" "OP_" n
-}

{-
--- Rename qualified type constructor.
renType (mn,fn) = (mkModName mn, mkTypeName fn)

-- Rename qualified data constructor.
renCons (mn,fn) = (mkModName mn, mkConName fn)

-- Rename qualified defined function.
renFunc (mn,fn) = (mkModName mn, mkFunName fn)
-}


{-
-- rename defined functions
mkFunName :: String -> String
mkFunName = replaceNonIdChars "c_" "op_"
-}


{-
-- rename data constructors
mkConName :: String -> String
mkConName n
  | n == "[]"     = "OP_Nil"
  | n == ":"      = "OP_Cons"
  | n == "()"     = "OP_Unit"
  | head n == '(' = "OP_Tuple" ++ show (length n - 1)
  | otherwise     = replaceNonIdChars "C_" "OP_" n

-- unrename data constructors
umkConName :: String -> String
umkConName n | mkConName oldCon =:= n = oldCon where oldCon free
{-
 | n == "OP_Nil"    = "[]"
 | n == "OP_Cons"   = ":"
 | n == "OP_Unit"   = "()"
 | take 2 n == "C_" = drop 2 n
 | otherwise        = n
-}
-}