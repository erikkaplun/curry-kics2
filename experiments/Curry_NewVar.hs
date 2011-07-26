{-# Language ExistentialQuantification #-}

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

lookupChoice :: IDSupply -> IO Decision
lookupChoice = readIORef . thisID


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


prdfs :: Curry a => a -> IO ()
prdfs x = match val_f choice_f failVal free_f app_f bind_f x
 where
  val_f                = print
  choice_f             = undefined
  failVal              = return ()
  free_f  supp         = lookupBind supp >>=  maybe (putStrLn "free") (prdfs . coerceTo x)
  app_f f supp vals    = lookupBind supp >>= maybe tryAll (prdfs . f)
                          where
                           tryAll = do  mapM_ (\val  -> writeIORef bind (Just val) >> prdfs (f val)) 
                                              vals
                                        writeIORef bind Nothing
                           bind   = getBindRef supp 
  bind_f f supp eq val = lookupBind supp >>= maybe (setBind supp val >> prdfs (f C_Success) 
                                                                     >> setBind supp Nothing)
                                                   (prdfs . f . eq)

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

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

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

goal1 = initSupply >>= \s -> prdfs (isSingleton (L_Free s))

goal2 = initSupply >>= \s -> prdfs $ let free = L_Free s in
  C_Cons C_True (C_Cons C_False (C_Cons C_True C_Nil)) =:= free &> isSingleton free 

goal3 = initSupply >>= \s -> prdfs $ let free = L_Free s in
  (isSingleton free =:= C_True) & (C_Cons C_True (C_Cons C_False (C_Cons C_True C_Nil)) =:= free)