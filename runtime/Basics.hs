{-# LANGUAGE MagicHash, CPP #-}

module Basics
  ( module Basics
  , module FailInfo
  , module PrimTypes
  , module Search
  , module Types
  ) where

import Control.Arrow (first)
import Data.Char     (ord)
import GHC.Exts      (Int#, Char#, chr#)

import Debug         (internalError)
import FailInfo
import PrimTypes
import Search
import Types

-- |Check the given argument for failure and add the function name and the
-- arguments to the trace in case of a failure.
failCheck :: NonDet a => String -> [String] -> a -> a
failCheck fun args x = case try x of
  Fail cd info -> failCons cd (traceFail fun args info)
  _            -> x

choice :: NonDet a => a -> a -> IDSupply -> Cover -> ConstStore -> a
choice x y s cd _ = let i = thisID s in i `seq` choiceCons cd i x y

-- -----------------------------------------------------------------------------
-- Auxiliaries for application of higher-order functions
-- -----------------------------------------------------------------------------

-- |Convert a deterministic function to a non-deterministic shape
nd :: (a -> Cover -> ConstStore -> b)
   -> a -> IDSupply -> Cover -> ConstStore -> b
nd f a _ cd cs = f a cd cs

-- |Make a higher order function take a cover depth and a constraint store
-- |after each argument.
acceptCs ::  (b -> c)-> (a -> b) -> a -> Cover -> ConstStore -> c
acceptCs cont f x _ _ = cont (f x)

-- |Wrap a deterministic function to a higher-order, non-deterministic function.
wrapDX :: (c -> b) -> (a -> Cover -> ConstStore -> c) -> Func a b
wrapDX wrap f = wrapNX wrap (nd f)

-- |Wrap a non-deterministic function to a higher-order,
-- non-deterministic function.
wrapNX :: (c -> b) -> (a -> IDSupply -> Cover -> ConstStore -> c) -> Func a b
wrapNX wrap f = Func (\a s cd cs -> wrap $ f a s cd cs)

-- |Apply a deterministic function to a value.
d_apply :: (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
d_apply f a cd cs = f a cd cs

-- |Apply a non-deterministic function to a value.
nd_apply :: NonDet b => Func a b -> a -> IDSupply -> Cover -> ConstStore -> b
nd_apply fun a s cd cs = d_dollar_bang apply fun cd cs
  where
  apply (Func f) cd' cs' = f a s cd' cs'
  apply _        _   _   = internalError "Basics.nd_apply.apply: no ground term"

-- -----------------------------------------------------------------------------
-- Auxilaries for function application to head normalform
-- -----------------------------------------------------------------------------

-- Apply a function to the head normal form.
d_dollar_bang :: (NonDet a, NonDet b) => (a -> Cover -> ConstStore -> b) -> a
  -> Cover -> ConstStore -> b
#ifdef TRY
d_dollar_bang = d_dollar_bang_try
#else
d_dollar_bang = d_dollar_bang_match
#endif

-- Apply a non-deterministic function to the head normal form.
nd_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a
  -> IDSupply -> Cover -> ConstStore -> b
#ifdef TRY
nd_dollar_bang = nd_dollar_bang_try
#else
nd_dollar_bang = nd_dollar_bang_match
#endif

-- Implementation of d_dollar_bang using try.
d_dollar_bang_try :: (NonDet a, NonDet b) => (a -> Cover -> ConstStore -> b)
  -> a -> Cover -> ConstStore -> b
d_dollar_bang_try f x cd cs = case try x of
  Fail     d info  -> failCons    d info
  Choice   d i a b -> choiceCons  d i (d_dollar_bang f a cd cs)
                                      (d_dollar_bang f b cd cs)
  Narrowed d i xs  -> choicesCons d i (map (\y -> d_dollar_bang f y cd cs) xs)
  Free     d i xs  -> f (choicesCons d i xs) cd cs
  Guard    d c e   -> guardCons   d c (d_dollar_bang f e cd $! addCs c cs)
  Val        y     -> f y cd cs

-- Implementation of nd_dollar_bang using try.
nd_dollar_bang_try :: (NonDet a, NonDet b) => (Func a b)
  -> a -> IDSupply -> Cover -> ConstStore -> b
nd_dollar_bang_try f x s cd cs = case try x of
  Fail     d info  -> failCons d info
  Choice   d i a b -> choiceCons  d i (nd_dollar_bang f a s cd cs)
                                      (nd_dollar_bang f b s cd cs)
  Narrowed d i xs  -> choicesCons d i
                      (map (\y -> nd_dollar_bang f y s cd cs) xs)
  Free     d i xs  -> nd_apply f      (choicesCons d i xs) s cd cs
  Guard    d c e   -> guardCons   d c (nd_dollar_bang f e s cd $! addCs c cs)
  Val        v     -> nd_apply f v s cd cs

-- Implementation of d_dollar_bang using match.
d_dollar_bang_match :: (NonDet a, NonDet b) => (a -> Cover -> ConstStore -> b)
  -> a -> Cover -> ConstStore -> b
d_dollar_bang_match f x cd cs
  = match hnfChoice hnfNarrowed hnfFree failCons hnfGuard (\y ->  f y cd cs) x
  where
  hnfChoice d i a b  = choiceCons  d i (d_dollar_bang f a cd cs)
                                       (d_dollar_bang f b cd cs)
  hnfNarrowed d i xs = choicesCons d i (map (\y -> d_dollar_bang f y cd cs) xs)
  hnfFree d i xs     = f (choicesCons d i xs) cd cs
  hnfGuard d c e     = guardCons   d c (d_dollar_bang f e cd $! addCs c cs)

-- Implementation of nd_dollar_bang using match.
nd_dollar_bang_match :: (NonDet a, NonDet b) => (Func a b)
  -> a -> IDSupply -> Cover -> ConstStore -> b
nd_dollar_bang_match f x s cd cs
  = match hnfChoice hnfNarrowed hnfFree failCons hnfGuard hnfVal x
  where
  hnfVal v         = nd_apply f v s cd cs
  -- TODO Do we better use leftSupply and rightSupply?
  hnfChoice d i a b  = choiceCons  d i (nd_dollar_bang f a s cd cs)
                                       (nd_dollar_bang f b s cd cs)
  hnfNarrowed d i xs = choicesCons d i
                       (map (\y -> nd_dollar_bang f y s cd cs) xs)
  hnfFree d i xs     = nd_apply f (choicesCons d i xs) s cd cs
  hnfGuard d c e     = guardCons   d c (nd_dollar_bang f e s cd $! addCs c cs)

-- -----------------------------------------------------------------------------
-- Pattern matching utilities for Literals
-- -----------------------------------------------------------------------------

nonAsciiChr :: Int# -> Char#
nonAsciiChr i = chr# i

matchChar :: NonDet a => [(Char, a)] -> BinInt -> Cover -> ConstStore -> a
matchChar rules cd cs = matchInteger (map (first ord) rules) cd cs

-- TODO@fre: use unboxed int

matchInteger :: NonDet a => [(Int, a)] -> BinInt -> Cover -> ConstStore -> a
matchInteger rules value cd cs = case value of
  Neg nat                -> matchNat (map (first abs)
                          $ filter ((< 0) . fst) rules) nat cd cs
  Zero                   -> maybe (failCons cd defFailInfo) id $ lookup 0 rules
  Pos nat                -> matchNat (filter ((> 0) . fst) rules) nat cd cs
  Choice_BinInt  d i l r -> narrow     d i (matchInteger rules l cd cs)
                                           (matchInteger rules r cd cs)
  Choices_BinInt d  i xs -> narrows cs d i (\x -> matchInteger rules x cd cs) xs
  Fail_BinInt    d  info -> failCons   d   info
  Guard_BinInt   d c int -> guardCons  d c
                            (matchInteger rules int cd $! addCs c cs)

matchNat :: NonDet a => [(Int, a)] -> Nat -> Cover -> ConstStore -> a
matchNat []    _     cd _  = failCons cd defFailInfo
matchNat rules value cd cs = case value of
  IHi                 -> maybe (failCons cd defFailInfo) id $ lookup 1 rules
  O nat               -> matchNat (map (first (`div` 2))
                       $ filter (evenPos . fst) rules) nat cd cs
                         where evenPos n = even n && (0 < n)
  I nat               -> matchNat (map (first (`div` 2))
                       $ filter (odd . fst) rules) nat cd cs
  Choice_Nat  d i l r -> narrow     d i (matchNat rules l cd cs)
                                        (matchNat rules r cd cs)
  Choices_Nat d i  xs -> narrows cs d i (\x -> matchNat rules x cd cs) xs
  Fail_Nat    d  info -> failCons   d info
  Guard_Nat   d c nat -> guardCons  d c $ matchNat rules nat cd $! addCs c cs

(&) :: C_Success -> C_Success -> Cover -> ConstStore -> C_Success
(&) s1 s2 _ cs = amp s1 s2 cs
  where
   amp C_Success                   s _   = s
   amp x@(Fail_C_Success _ _)      _ _   = x
   amp (Guard_C_Success cd c e)    s cs' =
         Guard_C_Success   cd c (amp e s $! addCs c cs')
   amp (Choice_C_Success cd i a b) s cs' =
         Choice_C_Success  cd i (amp a s cs') (amp b s cs')
   amp (Choices_C_Success cd i xs) s cs' =
         Choices_C_Success cd (narrowID i) (map (\x -> amp x s cs') xs)

{- interleaved (&) from Bernd
(&) :: C_Success -> C_Success -> C_Success
(&) C_Success        y = y
(&) x@Fail_C_Success _ = x
(&) x                y = maySwitch y x

maySwitch :: C_Success -> C_Success -> C_Success
maySwitch C_Success              x = x
maySwitch y@Fail_C_Success       _ = y
maySwitch (Guard_C_Success cs e) x = Guard_C_Success cs (x & e)
maySwitch y (Choice_C_Success i a b) = Choice_C_Success i (a & y) (b & y)
maySwitch y (Choices_C_Success i xs) = Choices_C_Success (narrowID i) (map (& y) xs)
maySwitch y (Guard_C_Success cs e)   = Guard_C_Success cs (e & y)
maySwitch y x                        = error $ "maySwitch: " ++ show y ++ " " ++ show x
-}
