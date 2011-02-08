-----------------------------------------------------------------------
--- Operations to change names of the original program into names
--- used in the target program
-----------------------------------------------------------------------
module Names where

import Char (isAlphaNum)
import List (intersperse)

mkConFunNameM :: String -> String
mkConFunNameM x = "consM_" ++ mkConNameM x 

mkConNameM :: String -> String
mkConNameM = replaceNonIdChars "CM_" "OPM_"

-- rename defined functions
mkFunNameM :: String -> String
mkFunNameM = replaceNonIdChars "cM_" "op_"

-- rename data constructors
mkConName :: String -> String
mkConName n | n=="[]"   = "OP_Nil"
            | n==":"    = "OP_Cons"
            | n=="()"   = "OP_Unit"
            | head n=='(' = "OP_Tuple"++show (length n - 1)
            | otherwise = replaceNonIdChars "C_" "OP_" n

-- rename type constructors
mkTypeName :: String -> String
mkTypeName n | n=="[]"   = "OP_List"
             | n=="()"   = "OP_Unit"
             | head n=='(' = "OP_Tuple"++show (length n - 1)
             | otherwise = replaceNonIdChars "C_" "OP_" n

-- rename modules
mkModName :: String -> String
mkModName = ("Curry_" ++)

mkExtModName :: String -> String
mkExtModName = ("External" ++)

compareNameForType :: String -> String
compareNameForType = (++ "_compare")

mkExtFunName :: String -> String
mkExtFunName = ("external_" ++) . mkFunNameM

-- | replaces characters that are not valid haskell identifiers,
-- | if there were no characters replaced, the first prefix,
-- | otherwise the snd prefix ist prepended
replaceNonIdChars :: String -> String -> String -> String
replaceNonIdChars pfxNonOp pfxOp str = 
   case strings of
     []  -> error "replaceNonIdChars: empty identifier"
     [s] -> if isAlphaNum (head str) 
               then pfxNonOp ++ s
               else pfxOp    ++ s
     _   -> pfxOp ++ concat (intersperse "_" strings)   

 where strings = separateAndReplace isAlphaNum showOpChar str
    

separateAndReplace :: (a -> Bool) -> (a -> [a]) -> [a] -> [[a]]
separateAndReplace pred f list =
  case rest of
    [] -> case sep of 
           [] -> []
           _  -> [sep]
    (x:xs) -> case sep of
               [] -> f x : separateAndReplace pred f xs
               _  -> sep : f x : separateAndReplace pred f xs
 where (sep,rest) = break  (not . pred) list


showOpChar :: Char -> String
showOpChar c = case c of
  '_' -> "underscore"
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
  _   -> error ("unexpected symbol: " ++ show c) 