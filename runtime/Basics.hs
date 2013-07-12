{-# LANGUAGE MagicHash #-}

module Basics
  ( module Basics
  , module PrimTypes
  , module Search
  , module Types
  ) where

import Data.Char(ord)
import GHC.Exts (Int#, Char#, chr#)

import Debug (internalError)
import PrimTypes
import Search
import Types

-- ---------------------------------------------------------------------------
-- Auxiliaries for non-determinism and higher order
-- ---------------------------------------------------------------------------

-- |Make a deterministic function non-deterministic
nd :: (a -> Cover -> ConstStore -> b) -> a -> IDSupply -> Cover -> ConstStore -> b
nd f a _ cd cs = f a cd cs

-- |Make higher order functions take a cover depths and a constraint store
-- |after each argument
acceptCs ::  (b -> c)-> (a -> b) -> a -> Cover -> ConstStore -> c
acceptCs cont f x _ _ = cont (f x)

-- |Wrap a deterministic function to a higher-order-non-deterministic function
wrapDX :: (c -> b) -> (a -> Cover -> ConstStore -> c) -> Func a b
wrapDX wrap f = wrapNX wrap (nd f)

-- |Wrap a non-deterministic function to a higher-order-non-deterministic
-- function
wrapNX :: (c -> b) -> (a -> IDSupply -> Cover -> ConstStore -> c) -> Func a b
wrapNX wrap f = Func (\a s cd cs -> wrap $ f a s cd cs)

-- |Apply a deterministic function
d_apply :: (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
d_apply f a cd cs = f a cd cs

-- |Apply a higher-order-non-deterministic function
nd_apply :: NonDet b => Func a b -> a -> IDSupply -> Cover -> ConstStore -> b
nd_apply fun a s cd cs = d_dollar_bang apply fun cd cs
  where
  apply (Func f) cd cs' = f a s cd cs'
  apply _        _  _ = internalError "Basics.nd_apply.apply: no ground term"

-- ---------------------------------------------------------------------------
-- Auxilaries for normalforms
-- ---------------------------------------------------------------------------

-- Apply a function to the head normal form
d_dollar_bang :: (NonDet a, NonDet b) => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
d_dollar_bang f x cd cs = match hnfChoice hnfNarrowed hnfFree failCons hnfGuard (\y ->  f y cd cs) x
  where
   hnfChoice d i a b  = choiceCons d i (d_dollar_bang f a cd cs) (d_dollar_bang f b cd cs)
   hnfNarrowed d i xs = choicesCons d i (map (\y -> d_dollar_bang f y cd cs) xs)
   hnfFree d i xs     = f (choicesCons d i xs) cd cs
   hnfGuard d c e     = guardCons d c (d_dollar_bang f e cd $! addCs c cs)

-- Apply a non-deterministic function to the head normal form
nd_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> Cover -> ConstStore -> b
nd_dollar_bang f x s cd cs = match hnfChoice hnfNarrowed hnfFree failCons hnfGuard hnfVal x
  where
   hnfVal v         = nd_apply f v s cd cs
   -- TODO Do we better use leftSupply and rightSupply?
   hnfChoice d i a b  = choiceCons d i (nd_dollar_bang f a s cd cs) (nd_dollar_bang f b s cd cs)
   hnfNarrowed d i xs = choicesCons d i (map (\y -> nd_dollar_bang f y s cd cs) xs)
   hnfFree d i xs     = nd_apply f (choicesCons d i xs) s cd cs
   hnfGuard d c e     = guardCons d c (nd_dollar_bang f e s cd $! addCs c cs)

-- ---------------------------------------------------------------------------
-- Pattern matching utilities for Literals
-- ---------------------------------------------------------------------------

nonAsciiChr :: Int# -> Char#
nonAsciiChr i = chr# i

matchChar :: NonDet a => [(Char,a)] -> BinInt -> Cover -> ConstStore -> a
matchChar rules cd cs = matchInteger (map (mapFst ord) rules) cd cs

-- TODO@fre: use unboxed int

matchInteger :: NonDet a => [(Int, a)] -> BinInt -> Cover -> ConstStore -> a
matchInteger rules (Neg nat) cd cs                =
  matchNat (map (mapFst abs) $ filter ((<0).fst) rules) nat cd cs
matchInteger rules Zero cd _                      =
  maybe (failCons cd defFailInfo) id $ lookup 0 rules
matchInteger rules (Pos nat) cd cs                = 
  matchNat (filter ((>0).fst) rules) nat cd cs
matchInteger rules (Choice_BinInt d i l r) cd cs =
  narrow d i (matchInteger rules l cd cs) (matchInteger rules r cd cs)
matchInteger rules (Choices_BinInt d i xs) cd cs =
  narrows cs d i (\x -> matchInteger rules x cd cs) xs
matchInteger _     (Fail_BinInt d info) _  _     = 
  failCons d info
matchInteger rules (Guard_BinInt d c int)  cd cs =
  guardCons d c (matchInteger rules int cd $! addCs c cs)

matchNat :: NonDet a => [(Int, a)] -> Nat -> Cover -> ConstStore -> a
matchNat []    _  cd _                    = failCons cd defFailInfo
matchNat rules IHi cd _                   = maybe (failCons cd defFailInfo) id $ lookup 1 rules
matchNat rules (O nat) cd cs              = 
  matchNat (map halfKey $ filter (evenPos.fst) rules) nat cd cs
  where
  evenPos n = even n && (0 < n)
matchNat rules (I nat) cd cs               = matchNat (map halfKey $ filter (odd.fst) rules) nat cd cs
matchNat rules (Choice_Nat d i l r) cd cs = narrow d i (matchNat rules l cd cs) (matchNat rules r cd cs)
matchNat rules (Choices_Nat d i xs) cd cs = narrows cs d i (\x -> matchNat rules x cd cs) xs
matchNat _     (Fail_Nat d info) _ _    = failCons d info
matchNat rules (Guard_Nat d c nat) cd cs  = guardCons d c $ matchNat rules nat cd $! addCs c cs

halfKey :: (Int,a) -> (Int,a)
halfKey =  mapFst (`div` 2)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

(&) :: C_Success -> C_Success -> Cover -> ConstStore -> C_Success
(&) s1 s2 _ cs = amp s1 s2 cs
  where
   amp C_Success                   s _  = s
   amp x@(Fail_C_Success _ _)      _ _  = x
   amp (Guard_C_Success cd c e)    s cs = 
         Guard_C_Success   cd c (amp e s $! addCs c cs)
   amp (Choice_C_Success cd i a b) s cs = 
         Choice_C_Success  cd i (amp a s cs) (amp b s cs)
   amp (Choices_C_Success cd i xs) s cs = 
         Choices_C_Success cd (narrowID i) (map (\x -> amp x s cs) xs)

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
