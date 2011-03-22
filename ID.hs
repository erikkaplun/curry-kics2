module ID
  ( Choice (..), ID (..), IDSupply
  , mkInt, leftID, rightID, narrowID
  , initSupply, leftSupply, rightSupply, thisID, freeID
  , lookupChoice, setChoice, setUnsetChoice
  ) where

import IDImpl

leftID, rightID :: ID -> ID
leftID  (FreeID s) = freeID (leftSupply s)
rightID (FreeID s) = freeID (rightSupply s)

narrowID :: ID -> ID
narrowID (FreeID s)     = Narrowed s
narrowID n@(Narrowed _) = n
narrowID i@(ID _)       = i

freeID :: IDSupply -> ID
freeID = FreeID

lookupChoice :: ID -> IO Choice
lookupChoice i = do
--   putStrLn $ "lookupChoice " ++ show i
  raw <- lookupChoiceRaw i
--   putStrLn $ "Raw result: " ++ show raw
  unchain raw
  where
    unchain (BoundTo j _) = lookupChoice j

    unchain (BindTo j)  = do -- lookupChoice j
      c <- lookupChoice j
      case c of
        NoChoice      -> return ()
        ChooseN _ num -> propagateBind i j num
        errChoice     -> error $ "ID.lookupChoice returned " ++ show errChoice
      return c

    unchain c           = do
--       putStrLn $ "ID.lookupChoice returned " ++ show c ++ " for ID " ++ show i
      return c

-- Propagate a binding of variable x to variable y for the next cnt child ids
-- x is expected to be either a free or a narrowed variable,
-- y is expected to be a free variable
propagateBind :: ID -> ID -> Int -> IO ()
propagateBind x@(FreeID xs) y@(FreeID ys) cnt = do
  setChoice x (BoundTo y cnt)
  sequence_ $ zipWith (\x' y' -> setChoice x' (BindTo y'))
                      (nextNIDs xs cnt) (nextNIDs ys cnt)
propagateBind x@(Narrowed xs) y@(FreeID ys) cnt = do
  setChoice x (BoundTo y cnt)
  sequence_ $ zipWith (\x' y' -> setChoice x' (BindTo y'))
                      (nextNIDs xs cnt) (nextNIDs ys cnt)
propagateBind errx erry errcnt = error $
  "propagateBind " ++ show errx ++ ' ' : show erry ++ ' ' : show errcnt

-- Compute a list of the next n free IDs for a given IDSupply s
nextNIDs :: IDSupply -> Int -> [ID]
nextNIDs s n
  | n <  0    = error $ "ID.nextNIDs with negative number " ++ show n
  | n == 0    = []
  | n == 1    = [freeID (leftSupply s)]
  | otherwise = nextNIDs' s n
  where
    nextNIDs' s n
      | n == 0    = []
      | n == 1    = [freeID s]
      | otherwise = nextNIDs' (leftSupply s) (n - halfn) ++ nextNIDs' (rightSupply s) halfn
        where halfn = n `div` 2

setUnsetChoice :: ID -> Choice -> IO (Maybe (IO ()))
setUnsetChoice i c = do
  j <- setChoiceGetID i c
  case c of
    BindTo _ -> return (Just (resetFreeVar j))
    _        -> return (Just (setChoice j NoChoice))

setChoiceGetID :: ID -> Choice -> IO ID
setChoiceGetID i c = lookupChoiceRaw i >>= unchain
  where
    unchain (BindTo j) = case c of
      ChooseN _ num -> do
        resId <- setChoiceGetID j c
        propagateBind i j num
        return resId
      _             -> setChoiceGetID j c
    unchain (BoundTo j _) = setChoiceGetID j c
    unchain _             = setChoice i c >> return i

resetFreeVar :: ID -> IO ()
resetFreeVar i = case i of
  ID _       -> error $ "resetFreeVar " ++ show i
  FreeID s   -> resetFreeVar' s
  Narrowed s -> resetFreeVar' s
  where
    resetFreeVar' s = lookupChoiceRaw i >>= propagate s

    propagate _ (BindTo _)  = setChoice i NoChoice
    propagate s (BoundTo _ num) = do
      setChoice i NoChoice
      mapM_ resetFreeVar $ nextNIDs s num
