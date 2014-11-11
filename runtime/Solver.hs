-- ---------------------------------------------------------------------------
-- Constraint Solving
-- ---------------------------------------------------------------------------
module Solver (Solution, solve) where

import Types

type Solution m a = m (Maybe (m (), a))

mkDecision :: Store m => ID -> Decision -> a -> Solution m a
mkDecision i d a = setUnsetDecision i d >>= \reset -> return $ Just (reset, a)

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

solve :: (Store m, NonDet a) => Cover -> Constraints -> a -> Solution m a
solve cd cnstrs val = solve' (getConstrList cnstrs) val
  where
  solve' []     a = mkSolution a
  solve' (c:cs) a = solveOne cd c a >>- solve' cs

solveOne :: (Store m, NonDet a) => Cover -> Constraint -> a -> Solution m a
solveOne _ (Unsolvable _) _ = noSolution
solveOne cd (ConstraintChoice d i lcs rcs) e = lookupDecision i >>= follow
  where
  follow ChooseLeft  = mkSolution $ guardCons  cd (StructConstr lcs) e
  follow ChooseRight = mkSolution $ guardCons  cd (StructConstr rcs) e
  follow NoDecision  = mkSolution $ choiceCons d i
                                    (guardCons cd (StructConstr lcs) e)
                                    (guardCons cd (StructConstr rcs) e)
  follow c           = error $ "Solver.solve.choose: CC:" ++ show c

solveOne cd cc@(ConstraintChoices d i css) e = lookupDecision i >>= follow
  where
  follow (ChooseN c _) = mkSolution $ guardCons cd (StructConstr (css !! c)) e
  follow NoDecision    = mkSolution $ choicesCons d i
                                    $ map (\cs -> guardCons cd (StructConstr cs) e) css
  follow (LazyBind cs) = mkSolution $ guardCons cd (StructConstr (cs ++ [cc])) e
  follow c             = error $ "Solver.solve.choose: CCs:" ++ show c

solveOne cd (i :=: cc) e = lookupDecision i >>= follow cc
  where
  -- 1st param: the (new) Choice which should be stored for i
  -- 2nd param: the (old) Choice for i in the store
  follow (LazyBind  _) NoDecision    = mkDecision i cc e
  follow (LazyBind cs1)(LazyBind cs2)= mkDecision i NoDecision
                                     $ guardCons cd (StructConstr (concatMap makeStrictCList (cs1++cs2))) e
  follow _             (LazyBind cs) = mkDecision i cc
                                     $ guardCons cd (StructConstr cs) e
  follow (LazyBind cs) _             = mkSolution
                                     $ guardCons cd (StructConstr cs) e
  follow (BindTo j)    ci            = lookupDecision j >>= \cj -> check cd i j ci cj e
  follow c             NoDecision    = mkDecision i c e
  follow c             ci            | c == ci   = mkSolution e
                                     | otherwise = noSolution

-- Check whether i can be bound to j and do so if possible
check :: (Store m, NonDet a) => Cover -> ID -> ID -> Decision -> Decision -> a
      -> Solution m a
check cd i j _               (LazyBind cs)   e = mkDecision j (BindTo i)
                                                  $ guardCons cd (StructConstr cs) e
check _  i j NoDecision      _               e = mkDecision i (BindTo j) e
check _  i j _               NoDecision      e = mkDecision j (BindTo i) e
check cd i j (ChooseN iN ip) (ChooseN jN jp) e
  = if iN == jN && ip == jp
    then mkSolution $ guardCons cd
        (StructConstr (zipWith (\ci cj -> ci :=: BindTo cj)
                               (nextNIDs i ip) (nextNIDs j ip)))
        e
    else noSolution
check _  _ _ ci              cj              e | ci == cj  = mkSolution e
                                               | otherwise = noSolution
