{-# LANGUAGE NoMonomorphismRestriction #-}
-- needed to define toplevel isomorphisms
module Giant where

class Eq n => N n where

  e :: n
  o,o',i,i' :: n -> n
  o_ :: n -> Bool

  e_,i_ :: n -> Bool
  e_ x = x == e
  i_ x = not (e_ x || o_ x)

  s,s' :: n -> n
  
  s x | e_ x = o x
  s x | o_ x = i (o' x)
  s x | i_ x = o (s (i' x))
  
  s' x | x == o e = e
  s' x | i_ x = o (i' x)
  s' x | o_ x = i (s' (o' x))

  allFrom :: n -> [n]
  allFrom x = iterate s x

  add :: n -> n -> n
  add x y | e_ x = y
  add x y | e_ y  = x
  add x y | o_ x && o_ y = i (add (o' x) (o' y))
  add x y | o_ x && i_ y = o (s (add (o' x) (i' y)))
  add x y | i_ x && o_ y = o (s (add (i' x) (o' y)))
  add x y | i_ x && i_ y = i (s (add (i' x) (i' y)))
  
  sub :: n -> n -> n
  sub x y | e_ y = x
  sub y x | o_ y && o_ x = s' (o (sub (o' y) (o' x))) 
  sub y x | o_ y && i_ x = s' (s' (o (sub (o' y) (i' x))))
  sub y x | i_ y && o_ x = o (sub (i' y) (o' x))  
  sub y x | i_ y && i_ x = s' (o (sub (i' y) (i' x))) 

  mul :: n -> n -> n
  mul x _ | e_ x = e
  mul _ y | e_ y = e
  mul x y = s (m (s' x) (s' y)) where
    m x y | e_ x = y
    m x y | o_ x = o (m (o' x) y)
    m x y | i_ x = s (add y  (o (m (i' x) y)))

  db,hf :: n -> n 
  db = s' . o
  hf = o' .s 

instance N Integer where
  e = 0
  
  o_ x = odd x
  
  o  x = 2*x+1
  o' x | odd x && x >  0 = (x-1) `div` 2
  
  i  x = 2*x+2
  i' x | even x && x > 0 = (x-2) `div` 2

view :: (N a,N b) => a -> b
view x | e_ x = e
view x | o_ x = o (view (o' x))
view x | i_ x = i (view (i' x))

n :: (N n) => n -> Integer
n = view

data T = T | V T [T] | W T [T] deriving (Eq,Show,Read)

instance N T where
  e = T
  
  o T = V T []
  o (V x xs) = V (s x) xs
  o (W x xs) = V T (x:xs)

  i T = W T []
  i (V x xs) = W T (x:xs)
  i (W x xs) = W (s x) xs
  
  o' (V T []) = T
  o' (V T (x:xs)) = W x xs
  o' (V x xs) = V (s' x) xs

  i' (W T []) = T
  i' (W T (x:xs)) = V x xs
  i' (W x xs) = W (s' x) xs
  
  o_ (V _ _ ) = True
  o_ _ = False

-- Trie representation for T
type TList = [T]

data TListMap a = TListMap (Maybe a) (TMap (TListMap a))

emptyTListMap :: TListMap a
emptyTListMap = TListMap Nothing emptyTMap

lookupTList :: TList -> TListMap a -> Maybe a
lookupTList []     (TListMap v _)    = v
lookupTList (t:ts) (TListMap _ tMap) = lookupT t tMap >>= lookupTList ts

insertTList :: TList -> a -> TListMap a -> TListMap a
insertTList tList x = updateTList tList (const (Just x))

deleteTList :: TList -> TListMap a -> TListMap a
deleteTList tList = updateTList tList (const Nothing)

updateTList :: TList -> (Maybe a -> Maybe a) -> TListMap a -> TListMap a
updateTList [] upd (TListMap v tMap) = TListMap (upd v) tMap
updateTList (t:ts) upd (TListMap v tMap) = TListMap v $
  updateT t (Just . updateTList ts upd . maybe emptyTListMap id) tMap

data TMap a = TMap (Maybe a) (TMap (TListMap a)) (TMap (TListMap a))

emptyTMap :: TMap a
emptyTMap = TMap Nothing emptyTMap emptyTMap

lookupT :: T -> TMap a -> Maybe a
lookupT T        (TMap v _ _)     = v
lookupT (V c ts) (TMap _ tMapV _) = lookupT c tMapV >>= lookupTList ts
lookupT (W c ts) (TMap _ _ tMapW) = lookupT c tMapW >>= lookupTList ts

insertT :: T -> a -> TMap a -> TMap a
insertT t = updateT t . const . Just

deleteT :: T -> TMap a -> TMap a
deleteT t = updateT t (const Nothing) 

updateT :: T -> (Maybe a -> Maybe a) -> TMap a -> TMap a
updateT T upd (TMap v tMapV tMapW) = TMap (upd v) tMapV tMapW
updateT (V c ts) upd (TMap v tMapV tMapW) = TMap v 
  (updateT c (Just . updateTList ts upd . maybe emptyTListMap id) tMapV) tMapW
updateT (W c ts) upd (TMap v tMapV tMapW) = TMap v tMapV
  (updateT c (Just . updateTList ts upd . maybe emptyTListMap id) tMapW)
