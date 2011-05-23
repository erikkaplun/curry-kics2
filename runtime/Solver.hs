-- ---------------------------------------------------------------------------
-- Constraint Solving
-- ---------------------------------------------------------------------------
module Solver
  ( Solution, SolutionTree (..), solves
  ) where

import ID
import Debug

-- |Type for an action to reset a choice made earlier
type Reset = IO ()

type Solution = IO SolutionTree

data SolutionTree
  = FailST
  | SuccessST Reset
  | ChoiceST  Reset Solution Solution
  | ChoicesST Reset [Solution]

mkSolution :: IO Reset -> Solution
mkSolution mkReset = mkReset >>= return . SuccessST

solved :: Solution
solved = return $ SuccessST $ return ()

unsolvable :: Solution
unsolvable = return FailST

(>>>) :: Solution -> Solution -> Solution
a >>> b = do
  tmra <- a
  case tmra of
    FailST -> unsolvable
    SuccessST ra -> do
      tmrb <- b
      case tmrb of
        FailST           -> ra >> unsolvable
        SuccessST rb     -> return $ SuccessST (rb >> ra)
        ChoiceST cr l r  -> return $ ChoiceST  (cr >> ra) l r
        ChoicesST cr sl  -> return $ ChoicesST (cr >> ra) sl
    ChoiceST cr l r -> return $ ChoiceST cr (l >>> b) (r >>> b)
    ChoicesST cr sl -> return $ ChoicesST cr (map (>>> b) sl)

solves :: [Constraint] -> Solution
solves [] = solved
solves (c:cs) = do
  trace $ "solving " ++ take 200 (show c)
  solve c >>> solves cs

solve :: Constraint -> Solution
solve Unsolvable = unsolvable
solve (ConstraintChoice i lcs rcs) = lookupChoice i >>= chooseCC
  where
    chooseCC ChooseLeft  = solves lcs
    chooseCC ChooseRight = solves rcs
    chooseCC NoChoice    = return $ ChoiceST
      (return ())
      (mkSolution (setUnsetChoice i ChooseLeft ) >>> solves lcs)
      (mkSolution (setUnsetChoice i ChooseRight) >>> solves rcs)
    chooseCC c           = error $ "ID.solve.chooseCC: " ++ show c
solve (ConstraintChoices i css) = lookupChoice i >>= chooseCCs
  where
    chooseCCs (ChooseN c _) = solves (css !! c)
    chooseCCs NoChoice      = return $
      ChoicesST (return ()) $ zipWith mkChoice [0 ..] css
    chooseCCs c           = error $ "ID.solve.chooseCCs: " ++ show c

    mkChoice n cs = mkSolution (setUnsetChoice i (ChooseN n (-1))) >>> solves cs
solve (i :=: cc) = lookupChoice i >>= choose cc
  where
  -- 1st param: the Choice which should be stored for i
  -- 2nd param: the Choice for i in the store

  -- store lazy binds for later use
  choose (LazyBind  _) NoChoice      = mkSolution (setUnsetChoice i cc)
  -- solve stored lazy binds when they are needed
  choose _             (LazyBind cs) = mkSolution (setUnsetChoice i cc) >>> solves cs
  choose (LazyBind cs) _             = solves cs
  choose (BindTo j)    ci            = lookupChoice j >>= check i j ci
  choose c             NoChoice      = mkSolution (setUnsetChoice i c)
  choose c             ci            = if c == ci then solved else unsolvable
--     choose _             (LazyBind cs) = setChoice i NoChoice >> solves cs >>> solve (i :=: cc)
--     choose (LazyBind cs) (LazyBind cs2) = solves cs >>> solves cs2
--     choose (LazyBind cs) (ChooseN _ _)  = solves cs
--     choose (ChooseN _ _) (LazyBind cs) = (setUnsetChoice i NoChoice >>> solves cs) >>> solve (i :=: cc)

-- Check whether i can be bound to j and do so if possible
check :: ID -> ID -> Choice -> Choice -> Solution
check i j _        (LazyBind cs)          = mkSolution (setUnsetChoice j (BindTo i)) >>> solves cs
check i j NoChoice _                      = mkSolution (setUnsetChoice i (BindTo j))
check i j _        NoChoice               = mkSolution (setUnsetChoice j (BindTo i))
check i j (ChooseN iN ip) (ChooseN jN jp) = if iN == jN && ip == jp
                                              then solves $ zipWith (\childi childj -> childi :=: BindTo childj)
                                                                    (nextNIDs i ip) (nextNIDs j ip)
                                              else unsolvable
check _ _ ci       cj                     = if ci == cj then solved else unsolvable

-- ---------------
-- Former approach
-- ---------------

--  -- Nothing -> Constraint is unsolvable
--  -- Just reset -> Constraint has been solved
-- type Solved = IO (Maybe (IO ()))
--
-- mkSolved :: IO (IO ()) -> Solved
-- mkSolved mkReset = mkReset >>= return . Just
--
-- solved :: Solved
-- solved = return (Just (return ()))
--
-- unsolvable :: Solved
-- unsolvable = return Nothing
--
-- (>>>) :: Solved -> Solved -> Solved
-- a >>> b = do
--   mra <- a
--   case mra of
--     Nothing -> return Nothing
--     Just ra -> do
--       mrb <- b
--       case mrb of
--         Nothing -> ra >> return Nothing
--         Just rb -> return (Just (ra >> rb))
