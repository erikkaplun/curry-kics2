-- ---------------------------------------------------------------------------
-- Constraint Solving
-- ---------------------------------------------------------------------------
module Solver (Solution, solves, solve) where

import ID
import Types

type Solution m a = m (Maybe (m (), a))

mkDecision :: Monad m => m (m ()) -> a -> Solution m a
mkDecision decide a = decide >>= \reset -> return $ Just (reset, a)

mkSolution :: Monad m => a -> Solution m a
mkSolution a = return $ Just (return (), a)

noSolution :: Monad m => Solution m a
noSolution = return Nothing

(>>-) :: Monad m => Solution m a -> (a -> Solution m b) -> Solution m b
ma >>- mbf = do
  mbSolutionA <- ma
  case mbSolutionA of
    Nothing          -> noSolution
    Just (resetA, a) -> do
      mbSolutionB <- mbf a
      case mbSolutionB of
        Nothing          -> resetA >> noSolution
        Just (resetB, b) -> return $ Just (resetB >> resetA, b)

solves :: (Store m, NonDet a) => [Constraint] -> a -> Solution m a
solves []     a = mkSolution a
solves (c:cs) a = solve c a >>- solves cs

solve :: (Store m, NonDet a) => Constraint -> a -> Solution m a
solve Unsolvable _ = noSolution
solve (ConstraintChoice i lcs rcs) e = lookupChoice i >>= choose
  where
  choose ChooseLeft  = mkSolution $ guardCons (StructConstr lcs) e
  choose ChooseRight = mkSolution $ guardCons (StructConstr rcs) e
  choose NoChoice    = mkSolution $ choiceCons i
                                    (guardCons (StructConstr lcs) e)
                                    (guardCons (StructConstr rcs) e)
  choose c           = error $ "Solver.solve.choose: CC:" ++ show c

solve cc@(ConstraintChoices i css) e = lookupChoice i >>= choose
  where
  choose (ChooseN c _) = mkSolution $ guardCons (StructConstr (css !! c)) e
  choose NoChoice      = mkSolution $ choicesCons i
                                    $ map (\cs -> guardCons (StructConstr cs) e) css
  choose (LazyBind cs) = mkSolution $ guardCons (StructConstr (cs ++ [cc])) e
  choose c             = error $ "Solver.solve.choose: CCs:" ++ show c

solve (i :=: cc) e = lookupChoice i >>= choose cc
  where
  -- 1st param: the (new) Choice which should be stored for i
  -- 2nd param: the (old) Choice for i in the store
  choose (LazyBind  _) NoChoice      = mkDecision (setUnsetChoice i cc) e
  choose _             (LazyBind cs) = mkDecision (setUnsetChoice i cc)
                                     $ guardCons (StructConstr cs) e
  choose (LazyBind cs) _             = mkSolution $ guardCons (StructConstr cs) e
  choose (BindTo j)    ci            = lookupChoice j >>= \cj -> check i j ci cj e
  choose c             NoChoice      = mkDecision (setUnsetChoice i c) e
  choose c             ci            = if c == ci then mkSolution e
                                                  else noSolution

-- Check whether i can be bound to j and do so if possible
check :: (Store m, NonDet a) => ID -> ID -> Choice -> Choice -> a -> Solution m a
check i j _               (LazyBind cs)   e
  = mkDecision (setUnsetChoice j (BindTo i)) $ guardCons (StructConstr cs) e
check i j NoChoice        _               e
  = mkDecision (setUnsetChoice i (BindTo j)) e
check i j _               NoChoice        e
  = mkDecision (setUnsetChoice j (BindTo i)) e
check i j (ChooseN iN ip) (ChooseN jN jp) e
  = if iN == jN && ip == jp
    then mkSolution $ guardCons
        (StructConstr (zipWith (\ci cj -> ci :=: BindTo cj)
                               (nextNIDs i ip) (nextNIDs j ip)))
        e
    else noSolution
check _ _ ci              cj              e
  = if ci == cj then mkSolution e else noSolution
