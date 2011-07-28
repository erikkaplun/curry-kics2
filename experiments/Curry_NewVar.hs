{-# Language ExistentialQuantification, Rank2Types #-}

import Data.IORef
import GHC.IO (unsafeDupableInterleaveIO)
import Unsafe.Coerce(unsafeCoerce)


--------------------------------------------------------------------------------
-- ID and Ref Supply
--------------------------------------------------------------------------------

data Decision = NoChoice | ChooseLeft | ChooseRight

defaultChoice = NoChoice

type ID = IORef Decision
data Binding = forall a . Binding (IORef (Maybe a))

data IDSupply = IDSupply ID Binding IDSupply IDSupply

instance Eq IDSupply where
  IDSupply i _ _ _ == IDSupply j _ _ _ = i == j

instance Show IDSupply where
  show _ = "supply"

initSupply :: IO IDSupply
initSupply = getPureSupply defaultChoice

{-# NOINLINE getPureSupply #-}
getPureSupply :: Decision -> IO IDSupply
getPureSupply def = do
  s1 <- unsafeDupableInterleaveIO (getPureSupply def)
  s2 <- unsafeDupableInterleaveIO (getPureSupply def)
  r  <- unsafeDupableInterleaveIO (newIORef def)
  b  <- unsafeDupableInterleaveIO (newIORef Nothing)
  return (IDSupply r (Binding b) s1 s2)

leftSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply _ _ s _) = s

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply _ _ _ s) = s

thisID :: IDSupply -> ID
thisID (IDSupply r _ _ _) = r

thisBind :: IDSupply -> Binding
thisBind (IDSupply _ b _ _) = b

getBindRef :: IDSupply -> IORef (Maybe a)
getBindRef (IDSupply _ (Binding ref) _ _) = unsafeCoerce ref

lookupBind :: forall a . IDSupply -> IO (Maybe a)
lookupBind (IDSupply _ (Binding b) _ _) = readIORef (unsafeCoerce b)

setBind :: IDSupply -> a -> IO ()
setBind (IDSupply _ (Binding b) _ _) = writeIORef (unsafeCoerce b) . Just

unsetBind :: IDSupply -> IO ()
unsetBind (IDSupply _ (Binding b) _ _) = writeIORef b Nothing

lookupChoice :: ID -> IO Decision
lookupChoice = readIORef

setChoice :: ID -> Decision -> IO ()
setChoice ref = writeIORef ref


-------------------------------------------------------------------------------
-- Search
-------------------------------------------------------------------------------

class Show a => Curry a where

  match :: (a -> b)                                                       -- function for values 
        -> (ID -> a -> a -> b)                                            -- function for choices
        -> b                                                              -- value for failures
        -> (IDSupply -> b)                                                -- function for free variables
        -> ((t -> a) -> IDSupply -> [t] -> b)                             -- function for application to free variables
        -> ((C_Success -> a) -> IDSupply -> (t -> C_Success) -> t -> b)   -- function for free variable bindings
        -> a
        -> b

  generate :: IDSupply -> [a]
  
  freeCons   :: IDSupply -> a
  choiceCons :: ID -> a -> a -> a
  failCons   :: a
  appCons    :: (t -> a) -> IDSupply -> [t] -> a
  bindCons   :: (C_Success -> a) -> IDSupply -> (t -> C_Success) -> t -> a
  
  ($!!) :: Curry b => (a -> b) -> a -> b
  
  (=:=) :: a -> a -> C_Success

  searchNF :: (forall b . Curry b => (b -> c) -> b -> c) -> (a -> c) -> a -> c


prdfs :: Curry a => a -> IO ()
prdfs = printdfs print

printdfs :: Curry a => (a -> IO ()) -> a -> IO ()
printdfs cont = printdfs' cont . (id $!!)

printdfs' :: Curry a => (a -> IO ()) -> a -> IO ()
printdfs' cont x = match val_f choice_f failVal free_f app_f bind_f x
 where
  val_f                = searchNF printdfs' cont
  choice_f i l r       = lookupChoice i >>= choose
                          where choose ChooseLeft  = printdfs' cont l
                                choose ChooseRight = printdfs' cont r
                                choose NoChoice    = do
                                  setChoice i ChooseLeft
                                  printdfs' cont l
                                  setChoice i ChooseRight
                                  printdfs' cont r
                                  setChoice i NoChoice
  failVal              = return ()
  free_f  supp         = lookupBind supp >>=  maybe (cont (freeCons supp)) (printdfs' cont . coerceTo x)
  app_f f supp vals    = lookupBind supp >>= maybe tryAll (printdfs cont . f)
                          where
                           tryAll = do  mapM_ (\val  -> writeIORef bind (Just val) >> printdfs cont (f val)) 
                                              vals
                                        writeIORef bind Nothing
                           bind   = getBindRef supp 
  bind_f f supp eq val = lookupBind supp >>= maybe (setBind supp val >> printdfs cont (f C_Success) 
                                                                     >> unsetBind supp)
                                                   (printdfs cont . f . eq)

coerceTo :: a -> b -> a
coerceTo _ = unsafeCoerce


-------------------------------------------------------------------------------
-- Data Structures
-------------------------------------------------------------------------------

data C_Success = C_Success
               | S_Choice ID C_Success C_Success
               | S_Fail
               | S_Free IDSupply
               | forall t . S_App (t -> C_Success) IDSupply [t]
               | forall t . S_Bind (C_Success -> C_Success) IDSupply (t -> C_Success) t
                 
instance Show C_Success where
  showsPrec _ C_Success       = showString "C_Success"
  showsPrec _ (S_Choice _ l r) = showChar '(' 
                               . shows l 
                               . showString " ? " 
                               . shows r 
                               . showChar ')'  
  showsPrec _ S_Fail           = showString "S_Fail"
  showsPrec _ (S_Free _)       = showString "S_Free"
  showsPrec _ (S_App _ _ _)    = showString "S_App"
  showsPrec _ (S_Bind _ _ _ _) = showString "S_Bind"
  
instance Curry C_Success where
  match val_f _        _       _      _     _ v@C_Success                  = val_f v
  match _     choice_f _       _      _     _ (S_Choice i l r)             = choice_f i l r
  match _     _        failVal _      _     _ S_Fail                       = failVal
  match _     _        _       free_f _     _ (S_Free ref)                 = free_f ref
  match _     _        _       _      app_f _ (S_App fun ref vals)         = app_f (unsafeCoerce fun) ref (unsafeCoerce vals)
  match _     _        _       _      _     bind_f (S_Bind fun ref eq val) = bind_f fun ref (unsafeCoerce eq) (unsafeCoerce val)

  generate _ = [C_Success]

  freeCons   = S_Free
  choiceCons = S_Choice
  failCons   = S_Fail
  appCons    = S_App
  bindCons   = S_Bind
  
  f $!! C_Success             = f C_Success
  f $!! S_Choice i l r        = choiceCons i (f $!! l) (f $!! r)  
  _ $!! S_Fail                = failCons
  f $!! var@(S_Free _)        = f var  
  f $!! S_App fun ref vals    = appCons (f . fun) ref vals --TODO: is $!! needed here? consider lazy bind  
  f $!! S_Bind fun ref eq val = bindCons (f . fun) ref eq val --TODO: s.o. 
  
  C_Success             =:= C_Success            = C_Success
  S_Fail                =:= _                    = S_Fail
  _                     =:= S_Fail               = S_Fail
  S_Choice i l r        =:= x                    = S_Choice i (l =:= x) (r =:= x) 
  S_Free ref            =:= x                    = (\x' -> S_Bind id ref (=:= x') x') $!! x 
  S_App fun ref vals    =:= x                    = S_App ((=:= x) . fun) ref vals
  S_Bind fun ref eq val =:= x                    = S_Bind ((=:= x) . fun) ref eq val
  x                     =:= S_Choice i l r       = S_Choice i (x =:= l) (x =:= r)
  x                     =:= S_Free ref           = (\x' -> S_Bind id ref  (=:= x') x') $!! x 
  x                     =:= S_App fun ref vals    = S_App ((x =:=) . fun) ref vals 
  x                     =:= S_Bind fun ref eq val = S_Bind ((=:= x) . fun) ref eq val

  searchNF _ cont C_Success = cont C_Success
  searchNF _ _ x = error ("Prelude.Success.searchNF: no constructor: " ++ (show x))
  
data C_Bool = C_True 
            | C_False 
            | B_Choice ID C_Bool C_Bool 
            | B_Fail 
            | B_Free IDSupply
            | forall t . B_App (t -> C_Bool) IDSupply [t]
            | forall t . B_Bind (C_Success -> C_Bool) IDSupply (t -> C_Success) t
              
instance Show C_Bool where
  showsPrec _ C_True           = showString "C_True"
  showsPrec _ C_False          = showString "C_False"
  showsPrec _ (B_Choice _ l r) = showChar '(' 
                               . shows l 
                               . showString " ? " 
                               . shows r 
                               . showChar ')'  
  showsPrec _ B_Fail           = showString "B_Fail"
  showsPrec _ (B_Free _)       = showString "B_Free"
  showsPrec _ (B_App _ _ _)    = showString "B_App"
  showsPrec _ (B_Bind _ _ _ _) = showString "B_Bind"

instance Curry C_Bool where
  match val_f _        _       _      _     _      v@C_True                = val_f v
  match val_f _        _       _      _     _      v@C_False               = val_f v
  match _     choice_f _       _      _     _      (B_Choice i l r)        = choice_f i l r
  match _     _        failVal _      _     _      B_Fail                  = failVal
  match _     _        _       free_f _     _      (B_Free ref)            = free_f ref
  match _     _        _       _      app_f _      (B_App fun ref vals)    = app_f (unsafeCoerce fun) ref (unsafeCoerce vals)
  match _     _        _       _      _     bind_f (B_Bind fun ref eq val) = bind_f fun ref (unsafeCoerce eq) (unsafeCoerce val)

  generate _ = [C_True,C_False]

  freeCons   = B_Free
  choiceCons = B_Choice
  failCons   = B_Fail
  appCons    = B_App
  bindCons   = B_Bind
  
  f $!! C_True                = f C_True 
  f $!! C_False               = f C_False
  f $!! B_Choice i l r        = choiceCons i (f $!! l) (f $!! r)  
  _ $!! B_Fail                = failCons
  f $!! var@(B_Free _)        = f var  
  f $!! B_App fun ref vals    = appCons (f . fun) ref vals --TODO: is $!! needed here? consider lazy bind  
  f $!! B_Bind fun ref eq val = bindCons (f . fun) ref eq val --TODO: s.o. 
  
  C_True                =:= C_True                = C_Success
  C_False               =:= C_False               = C_Success
  B_Fail                =:= _                     = S_Fail
  _                     =:= B_Fail                = S_Fail
  B_Choice i l r        =:= x                     = S_Choice i (l =:= x) (r =:= x) 
  B_Free ref            =:= x                     = (\x' -> S_Bind id ref (x' =:=) x') $!! x 
  B_App fun ref vals    =:= x                     = S_App ((=:= x) . fun) ref vals
  B_Bind fun ref eq val =:= x                     = S_Bind ((=:= x) . fun) ref eq val
  x                     =:= B_Choice i l r        = S_Choice i (x =:= l) (x =:= r)
  x                     =:= B_Free ref            = (\x' -> S_Bind id ref (x' =:=) x') $!! x
  x                     =:= B_App fun ref vals    = S_App ((x =:=) . fun) ref vals 
  x                     =:= B_Bind fun ref eq val = S_Bind ((=:= x) . fun) ref eq val
  _                     =:= _                     = S_Fail  
 

  searchNF _ cont C_True  = cont C_True
  searchNF _ cont C_False = cont C_False
  searchNF _ _ x = error ("Prelude.Bool.searchNF: no constructor: " ++ (show x)) 

data C_List a = C_Cons a (C_List a) 
              | C_Nil 
              | L_Choice ID (C_List a) (C_List a) 
              | L_Fail 
              | L_Free IDSupply
              | forall t . L_App (t -> C_List a) IDSupply [t]
              | forall t . L_Bind (C_Success -> C_List a) IDSupply (t -> C_Success) t
                
instance Show a => Show (C_List a) where
  showsPrec _ C_Nil            = showString "C_Nil"
  showsPrec _ (C_Cons h t)     = showString "(C_Cons " 
                              . shows h 
                              . showChar ' ' 
                              . shows t 
                              . showChar ')' 
  showsPrec _ (L_Choice _ l r) = showChar '(' 
                               . shows l 
                               . showString " ? " 
                               . shows r 
                               . showChar ')'  
  showsPrec _ L_Fail           = showString "L_Fail"
  showsPrec _ (L_Free _)       = showString "L_Free"
  showsPrec _ (L_App _ _ _)    = showString "L_App"
  showsPrec _ (L_Bind _ _ _ _) = showString "L_Bind"

instance Curry a => Curry (C_List a) where
  match val_f _        _       _      _     _      v@C_Nil                 = val_f v
  match val_f _        _       _      _     _      v@(C_Cons _ _)          = val_f v
  match _     choice_f _       _      _     _      (L_Choice i l r)        = choice_f i l r
  match _     _        failVal _      _     _      L_Fail                  = failVal
  match _     _        _       free_f _     _      (L_Free sup)            = free_f sup
  match _     _        _       _      app_f _      (L_App fun ref vals)    = app_f (unsafeCoerce fun) ref (unsafeCoerce vals)
  match _     _        _       _      _     bind_f (L_Bind fun ref eq val) = bind_f fun ref (unsafeCoerce eq) (unsafeCoerce val)

  generate s = [C_Nil, C_Cons (freeCons (leftSupply s)) (L_Free (rightSupply s))]

  freeCons   = L_Free
  choiceCons = L_Choice
  failCons   = L_Fail
  appCons    = L_App
  bindCons   = L_Bind
  
  f $!! C_Nil                 = f C_Nil
  f $!! C_Cons x xs           =  (\x' ->(\xs' -> f (C_Cons x' xs')) $!! xs) $!! x  
  f $!! L_Choice i l r        = choiceCons i (f $!! l) (f $!! r)
  f $!! L_Fail                = failCons
  f $!! var@(L_Free sup)      = f var 
  f $!! L_App fun ref vals    = appCons (f . fun) ref vals -- TODO: is $!! needed?
  f $!! L_Bind fun ref eq val = bindCons (f . fun) ref eq val -- s.o.
  
  C_Nil                 =:= C_Nil                 = C_Success
  C_Cons x xs           =:= C_Cons y ys           = (x =:= y) & (xs =:= ys)
  L_Fail                =:= _                     = S_Fail
  _                     =:= L_Fail                = S_Fail
  L_Choice i l r        =:= x                     = S_Choice i (l =:= x) (r =:= x) 
  L_Free ref            =:= x                     = (\x' -> S_Bind id ref (x' =:=) x') $!! x 
  L_App fun ref vals    =:= x                     = S_App ((=:= x) . fun) ref vals
  L_Bind fun ref eq val =:= x                     = S_Bind ((=:= x) . fun) ref eq val
  x                     =:= L_Choice i l r        = S_Choice i (x =:= l) (x =:= r)
  x                     =:= L_Free ref            = (\x' -> S_Bind id ref (x' =:=) x') $!! x
  x                     =:= L_App fun ref vals    = S_App ((x =:=) . fun) ref vals 
  x                     =:= L_Bind fun ref eq val = S_Bind ((=:= x) . fun) ref eq val
  _                     =:= _                     = S_Fail

  searchNF _ cont C_Nil = cont C_Nil
  searchNF search cont (C_Cons x1 x2) = search (\y1 -> search (\y2 -> cont (C_Cons y1 y2)) x2) x1
  searchNF _ _ x = error ("Prelude.List.searchNF: no constructor: " ++ (show x))  

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

(?) :: Curry a => a -> a -> IDSupply -> a
x ? y = \s -> choiceCons (thisID s) x y 

(&) :: C_Success -> C_Success -> C_Success
C_Success           & x = x
S_Choice i l r      & x = S_Choice i (l & x) (r & x) 
S_Fail              & _ = S_Fail
S_Free ref          & x = S_App (& x) ref (generate ref)
S_App f ref vals    & x = S_App ((& x) . f) ref vals
S_Bind f ref eq val & x = S_Bind ((& x) . f) ref eq val

(&>) :: Curry a => C_Success -> a -> a
C_Success           &> x = x
S_Choice i l r      &> x = choiceCons i (l &> x) (r &> x) 
S_Fail              &> _ = failCons
S_Free ref          &> x = appCons (&> x) ref (generate ref)
S_App f ref vals    &> x = appCons ((&> x) . f) ref vals
S_Bind f ref eq val &> x = bindCons ((&> x) . f) ref eq val

--------------------------------------------------------------------------------
-- Tests  
--------------------------------------------------------------------------------

{- Curry

isSingleton []      = False
isSingleton [_]     = True
isSingleton (_:_:_) = False 

-}

isSingleton :: C_List C_Bool -> C_Bool
isSingleton l = case l of
   C_Nil               -> C_False
   C_Cons x xs         -> isSingleton' xs
   L_Choice i xs ys    -> B_Choice i (isSingleton xs) (isSingleton ys)
   L_Fail              -> B_Fail
   L_Free ref          -> B_App isSingleton ref (generate ref)
   L_App f ref vals    -> B_App (isSingleton . f) ref vals
   L_Bind f ref eq val -> B_Bind (isSingleton . f) ref eq val


isSingleton' l = case l of
  C_Nil               -> C_True
  C_Cons _ _          -> C_False
  L_Choice i xs ys    -> B_Choice i (isSingleton' xs) (isSingleton' ys)
  L_Fail              -> B_Fail
  L_Free ref          -> B_App isSingleton' ref (generate ref)
  L_App f ref vals    -> B_App (isSingleton' . f) ref vals
  L_Bind f ref eq val -> B_Bind (isSingleton' . f) ref eq val
  
c_repeat :: Curry a => a -> C_List a
c_repeat x = C_Cons x (c_repeat x)

goal1 = initSupply >>= \s -> prdfs (isSingleton (L_Free s)) -- C_False, C_True, C_False

goal2 = initSupply >>= \s -> prdfs $ let free = L_Free s in
  C_Cons C_True (C_Cons C_False (C_Cons C_True C_Nil)) =:= free &> isSingleton free -- C_False

goal3 = initSupply >>= \s -> prdfs $ let free = L_Free s in
  (isSingleton free =:= C_True) & (C_Cons C_True (C_Cons C_False (C_Cons C_True C_Nil)) =:= free) -- no solution

goal4 = prdfs (C_True =:= C_True) -- C_Success
goal5 = prdfs (C_True =:= C_False) -- no solution
goal6 = initSupply >>= \s -> prdfs $ let free = B_Free s in free =:= C_True -- C_Success
goal7 = initSupply >>= \s -> prdfs $ let free = B_Free s in (free =:= C_True) &> free -- C_True
goal8 = initSupply >>= \s -> prdfs $ let free = B_Free s in  (free =:= C_True) &> C_Cons free (C_Cons free C_Nil) -- [True,True]
goal9  = initSupply >>= \s -> prdfs $ let free = B_Free s in (C_Cons ((free =:= C_True) &> free) (C_Cons free C_Nil))  -- [True,True]
goal10 = initSupply >>= \s -> prdfs $ let free = B_Free s in (C_Cons free (C_Cons ((free =:= C_True) &> free) C_Nil)) -- [True,True]


uni :: C_Bool -> C_Bool -> C_Success
uni x y = x =:= y

goal11 = initSupply >>= \s -> prdfs $ let free1 = B_Free (leftSupply s)
                                          free2 = B_Free (rightSupply s) in 
                                      (uni free1 free2) -- C_Success

goal12 = initSupply >>= \s -> prdfs $ let free1 = B_Free (leftSupply s)
                                          free2 = B_Free (rightSupply s) in
     (free1 =:= free2    
      &> free1 =:= C_True
      &> C_Cons free1 (C_Cons free2 C_Nil)) -- [True,True]


goal13 = initSupply >>= \s -> prdfs $ let free1 = B_Free (leftSupply s)
                                          free2 = B_Free (rightSupply s) in
      (free1 =:= free2  
      &> free1 =:= C_True
      &> C_Cons free1 (C_Cons free2 C_Nil)) -- [True,True]


goal14 = initSupply >>= \s -> prdfs $ let x = B_Free (leftSupply s)
                                          y = B_Free (leftSupply (rightSupply s)) 
                                          z = B_Free (rightSupply (rightSupply s)) in
       (x =:= y
       &> x =:= z
       &> y=:=C_False &> C_Cons x (C_Cons y C_Nil)) -- [False,False]


goal15 = initSupply >>= \s -> prdfs $ let x = B_Free (leftSupply s)
                                          y = B_Free (leftSupply (rightSupply s)) 
                                          z = B_Free (rightSupply (rightSupply s)) in
       (x =:= y     
       &> x =:= z
       &> x=:= C_False
       &> C_Cons z (C_Cons x (C_Cons y C_Nil))) -- [False,False,False]


goal16 = initSupply >>= \s -> prdfs $ let x = B_Free (leftSupply s)
                                          y = B_Free (leftSupply (rightSupply s)) 
                                          z = B_Free (rightSupply (rightSupply s)) in
       (  x =:= y     
       &> x =:= z
       &> z =:= C_False &> C_Cons x (C_Cons z (C_Cons y C_Nil))) -- [False,False,False]

goal17 = initSupply >>= \s -> prdfs $ let x = B_Free (leftSupply s)
                                          y = B_Free (leftSupply (rightSupply s)) 
                                          z = B_Free (rightSupply (rightSupply s)) in
       (x =:= y      
       &> x =:= z
       &> z=:= C_False
       &> y=:= C_False
       &> C_Cons x (C_Cons y (C_Cons z C_Nil))) -- [False,False,False]


goal18 = initSupply >>= \s -> prdfs $ let x = B_Free (leftSupply s)
                                          y = B_Free (rightSupply s) in 
         (x=:=y &> y=:= C_False &> x) -- False

goal19 = initSupply >>= \s -> prdfs $ let x = B_Free (leftSupply s)
                                          y = B_Free (leftSupply (rightSupply s)) in
   (x=:= ((y ? C_True)(rightSupply (rightSupply s))) &> y=:= C_False &> x) -- False, True


-- complex types

goal20 =  initSupply >>= \s -> prdfs $ let x  = (L_Free s :: C_List C_Bool) in (x =:= C_Nil &> C_Cons C_True x) -- [True]
goal21 =  initSupply >>= \s -> prdfs $ let x = (L_Free s :: C_List C_Bool) in (x=:= C_Cons C_True C_Nil &> x) --[True]

goal22 = initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                          y = (L_Free (rightSupply s) :: C_List C_Bool)in
 (x=:=y &> (y=:= C_Cons C_True C_Nil &> x)) -- [True]

f (C_Cons x xs) = f' x xs
f (L_Choice i xs ys)      = S_Choice i (f xs) (f ys)
f (L_Free ref)            = S_App f ref (generate ref)
f (L_App fun ref vals)    = S_App (f . fun) ref vals
f (L_Bind fun ref eq val) = S_Bind (f . fun) ref eq val
f _                       = S_Fail


f' C_False zs = f'' zs
f' (B_Choice i xs ys) zs     = S_Choice i (f' xs zs) (f' ys zs)
f' (B_Free ref) zs           = S_App (flip f' zs) ref (generate ref)
f' (B_App fun ref vals) zs   = S_App ((flip f' zs) . fun) ref vals
f' (B_Bind fun ref eq val)zs = S_Bind ((flip f' zs) . fun) ref eq val
f' _     _                   = S_Fail

f'' C_Nil = C_Success
f'' (L_Choice i xs ys)      = S_Choice i (f'' xs) (f'' ys)
f'' (L_Free ref)            = S_App f'' ref (generate ref)
f'' (L_App fun ref vals)    = S_App (f'' . fun) ref vals
f'' (L_Bind fun ref eq val) = S_Bind (f'' . fun) ref eq val
f'' _                       = S_Fail

goal23 = initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                          y = (L_Free (rightSupply s) :: C_List C_Bool)in
  (x=:=y &> y=:= C_Cons C_True C_Nil &> f x &> x) -- no solution

g (C_Cons x xs) = g' x xs
g (L_Choice i xs ys)      = S_Choice i (g xs) (g ys)
g (L_Free ref)            = S_App g ref (generate ref)
g (L_App fun ref vals)    = S_App (g . fun) ref vals
g (L_Bind fun ref eq val) = S_Bind (g . fun) ref eq val
g _                       = S_Fail


g' C_True zs = g'' zs
g' (B_Choice i xs ys) zs     = S_Choice i (g' xs zs) (g' ys zs)
g' (B_Free ref) zs           = S_App (flip g' zs) ref (generate ref)
g' (B_App fun ref vals) zs   = S_App ((flip g' zs) . fun) ref vals
g' (B_Bind fun ref eq val)zs = S_Bind ((flip g' zs) . fun) ref eq val
g' _  _                      = S_Fail

g'' C_Nil                   = C_Success
g'' (L_Choice i xs ys)      = S_Choice i (g'' xs) (g'' ys)
g'' (L_Free ref)            = S_App g'' ref (generate ref)
g'' (L_App fun ref vals)    = S_App (g'' . fun) ref vals
g'' (L_Bind fun ref eq val) = S_Bind (g'' . fun) ref eq val
g'' _                       = S_Fail


goal24 =   initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                            y = (L_Free (rightSupply s) :: C_List C_Bool) in
 (x=:=y &> y=:= C_Cons C_True C_Nil &> g x &> x) --[True]




goal25 = initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply (leftSupply s)) :: C_List C_Bool)
                                          y = (L_Free (rightSupply s) :: C_List C_Bool) in  
  (x =:= ((y ? (C_Cons C_False C_Nil))(rightSupply (leftSupply s))) &> y =:= C_Cons C_True C_Nil &> x) --[True],[False]

uni2 :: C_List C_Bool -> C_List C_Bool -> C_Success
uni2 x y = x =:= y

goal26 = initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                          y = (B_Free (rightSupply s) :: C_Bool)
 in  (uni2 x (C_Cons y C_Nil)) -- C_Success


goal27 = initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                          y = (B_Free (rightSupply s) :: C_Bool) in
  (uni2 x (C_Cons y C_Nil) &> x) --  [y]

goal28 = initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                          y = (B_Free (rightSupply s) :: C_Bool) in
 (x =:= C_Cons y C_Nil &> y=:= C_True &> x)  --[True]

goal29 = initSupply >>= \s -> prdfs $ let ls = (leftSupply s)
                                          rs = (rightSupply s)
                                          lls = (leftSupply ls)
                                          rls = (rightSupply ls)
                                          lrs = (leftSupply  rs)
                                          rrs = (rightSupply rs)
                                          llls = (leftSupply lls)
                                          rlls = (rightSupply lls)
                                          x = (L_Free rls :: C_List C_Bool)
                                          y = (B_Free lrs :: C_Bool)
                                          z = (L_Free rrs :: C_List C_Bool)
                                          z1 = (B_Free llls :: C_Bool)
                                          z2 = (L_Free rlls :: C_List C_Bool) 
                                          in
  (x =:= C_Cons y z &> x=:= C_Cons C_False (C_Cons z1 z2) &> z2 =:= C_Nil &> x) --[False,z1]



goal30 = initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply (leftSupply s)) :: C_List C_Bool)
                                          y = (B_Free (rightSupply s) :: C_Bool) in
 (x =:= C_Cons ((y ? C_True)(rightSupply (leftSupply s))) C_Nil &> y=:= C_False &> x) -- [False],[True]



goal31 =  initSupply >>= \s -> prdfs $ let x = B_Free (leftSupply s)
                                           y = B_Free (leftSupply (rightSupply s)) 
                                           z = B_Free (rightSupply (rightSupply s)) in
  (C_Cons x (C_Cons C_True (C_Cons z C_Nil)) =:= C_Cons C_False (C_Cons y (C_Cons y C_Nil)) 
  &> C_Cons x (C_Cons y (C_Cons z C_Nil))) --[False,True,True]

goal32 =  initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                           y = (L_Free (rightSupply s) :: C_List C_Bool) in 
  (x =:= (y =:= C_Cons C_True C_Nil &> y) &> x) --[True]

goal33 =  initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                           y = (L_Free (rightSupply s) :: C_List C_Bool) in 
  (x =:= (C_Cons C_True (y =:= C_Nil &> y)) &> x) -- [True]

goal34 =  initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                           y = (L_Free (rightSupply s) :: C_List C_Bool) in
  (x =:= C_Cons C_True C_Nil &> y =:= C_Cons C_False C_Nil &> x =:= y) -- no solution

append :: Curry a => C_List a -> C_List a -> C_List a
append C_Nil                 zs = zs
append (C_Cons x xs)         zs = C_Cons x (append xs zs)
append (L_Choice i xs ys)    zs = L_Choice i (append xs zs) (append ys zs)
append L_Fail                _  = L_Fail
append (L_Free ref)          zs = L_App (flip append zs) ref (generate ref)
append (L_App f ref vals)    zs = L_App ((flip append zs) . f) ref vals
append (L_Bind f ref eq val) zs = L_Bind ((flip append zs) . f) ref eq val


goal35 = initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                          y = (L_Free (rightSupply s) :: C_List C_Bool) in 
  (x =:= C_Nil &> append y (C_Cons C_False C_Nil) =:= x) -- no solution

goal36 = initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                          y1 = B_Free (leftSupply (rightSupply s))
                                          y2 = (L_Free (rightSupply (rightSupply s)) :: C_List C_Bool) in 
 (x =:= C_Nil &> C_Cons y1 (append y2 (C_Cons C_False C_Nil)) =:= x) -- no solution
goal37 = initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                          y2 = (L_Free(rightSupply s) :: C_List C_Bool) in
 (x =:= C_Nil &> append y2  (C_Cons C_False C_Nil) =:= x) -- no solution

goal38 =  initSupply >>= \s -> prdfs $ let x = (L_Free (leftSupply s) :: C_List C_Bool)
                                           y1 = B_Free (rightSupply s) in
   (x =:= C_Nil &> (C_Cons y1 (C_Cons C_False C_Nil)) =:= x) -- no solution
