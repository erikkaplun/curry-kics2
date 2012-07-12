-- 1. isPrefixOf, isSuffixOf

isPrefixOf :: [a] -> [a] -> Success
isPrefixOf a b = a++s =:= b where s free

isSuffixOf :: [a] -> [a] -> Success
isSuffixOf a b = p++a =:= b where p free

-- 2. prefix, suffix

prefix :: [a] -> [a]
prefix a | p++s =:= a = p where p,s free

suffix :: [a] -> [a]
suffix a | p++s =:= a = s where p,s free

-- 3. isSublistOf,sublist

isSublistOf :: [a] -> [a] -> Success
isSublistOf a b = isPrefixOf (p++a) b 
                & isSuffixOf (a++s) b where p,s free

sublist :: [a] -> [a]
sublist a | prefix a ++ v ++ suffix a =:= a = v where v free

-- Beziehung: Die Constraints lassen sich durch die Operationen ausdrücken und andersherum.

isPrefixOf2 :: [a] -> [a] -> Success
isPrefixOf2 a b = a =:= prefix b

isSuffixOf2 :: [a] -> [a] -> Success
isSuffixOf2 a b = a =:= suffix b

prefix2 :: [a] -> [a]
prefix2 a | isPrefixOf p a = p where p free

suffix2 :: [a] -> [a]
suffix2 a | isSuffixOf s a = s where s free

isSublistOf2 :: [a] -> [a] -> Success
isSublistOf2 a b = a =:= sublist b

sublist2 :: [a] -> [a]
sublist2 a | isSublistOf s a = s where s free
