{-# LANGUAGE MagicHash #-}

module Basics
  ( module Basics
  , module FailInfo
  , module PrimTypes
  , module Search
  , module Types
  ) where

import Data.Char(ord)
import GHC.Exts (Int#, Char#, chr#)

import Debug (internalError)
import FailInfo
import PrimTypes
import Search
import Types

failCheck :: NonDet a => String -> [String] -> a -> a
failCheck fun args x = case try x of
  Fail cd info -> failCons cd (traceFail fun args info)
  _            -> x

-- ---------------------------------------------------------------------------
-- Auxiliaries for non-determinism and higher order
-- ---------------------------------------------------------------------------

-- |Make a deterministic function non-deterministic
nd :: (a -> ConstStore -> b) -> a -> IDSupply -> ConstStore -> b
nd f a _ cs = f a cs

-- |Make higher order functions take a constraint store after each argument
acceptCs ::  (b -> c)-> (a -> b) -> a -> ConstStore -> c
acceptCs cont f x _ = cont (f x)

-- |Wrap a deterministic function to a higher-order-non-deterministic function
wrapDX :: (c -> b) -> (a -> ConstStore -> c) -> Func a b
wrapDX wrap f = wrapNX wrap (nd f)

-- |Wrap a non-deterministic function to a higher-order-non-deterministic
-- function
wrapNX :: (c -> b) -> (a -> IDSupply -> ConstStore -> c) -> Func a b
wrapNX wrap f = Func (\a s cs -> wrap $ f a s cs)

-- |Apply a deterministic function
d_apply :: (a -> ConstStore -> b) -> a -> ConstStore -> b
d_apply f a cs = f a cs

-- |Apply a higher-order-non-deterministic function
nd_apply :: NonDet b => Func a b -> a -> IDSupply -> ConstStore -> b
nd_apply fun a s cs = d_dollar_bang apply fun cs
  where
  apply (Func f) cs' = f a s cs'
  apply _          _ = internalError "Basics.nd_apply.apply: no ground term"

-- ---------------------------------------------------------------------------
-- Auxilaries for normalforms
-- ---------------------------------------------------------------------------

-- Apply a function to the head normal form
d_dollar_bang :: (NonDet a, NonDet b) => (a -> ConstStore -> b) -> a -> ConstStore -> b
d_dollar_bang f x cs = match hnfChoice hnfNarrowed hnfFree failCons hnfGuard (flip f cs) x
  where
   hnfChoice cd i a b  = choiceCons cd i (d_dollar_bang f a cs) (d_dollar_bang f b cs)
   hnfNarrowed cd i xs = choicesCons cd i (map (\y -> d_dollar_bang f y cs) xs)
   hnfFree cd i xs     = f (choicesCons cd i xs) cs
   hnfGuard cd c e     = guardCons cd c (d_dollar_bang f e $! addCs c cs)

-- Apply a non-deterministic function to the head normal form
nd_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> ConstStore -> b
nd_dollar_bang f x s cs = match hnfChoice hnfNarrowed hnfFree failCons hnfGuard hnfVal x
  where
   hnfVal v         = nd_apply f v s cs
   -- TODO Do we better use leftSupply and rightSupply?
   hnfChoice cd i a b  = choiceCons cd i (nd_dollar_bang f a s cs) (nd_dollar_bang f b s cs)
   hnfNarrowed cd i xs = choicesCons cd i (map (\y -> nd_dollar_bang f y s cs) xs)
   hnfFree cd i xs     = nd_apply f (choicesCons cd i xs) s cs
   hnfGuard cd c e     = guardCons cd c (nd_dollar_bang f e s $! addCs c cs)

-- ---------------------------------------------------------------------------
-- Pattern matching utilities for Literals
-- ---------------------------------------------------------------------------

nonAsciiChr :: Int# -> Char#
nonAsciiChr i = chr# i

matchChar :: NonDet a => [(Char,a)] -> BinInt -> ConstStore -> a
matchChar rules cs = matchInteger (map (mapFst ord) rules) cs

-- TODO@fre: use unboxed int

matchInteger :: NonDet a => [(Int, a)] -> BinInt -> ConstStore -> a
matchInteger rules (Neg nat) cs                =
  matchNat (map (mapFst abs) $ filter ((<0).fst) rules) nat cs
matchInteger rules Zero _                      =
  maybe (failCons 0 defFailInfo) id $ lookup 0 rules
matchInteger rules (Pos nat) cs                = 
  matchNat (filter ((>0).fst) rules) nat cs
matchInteger rules (Choice_BinInt cd i l r) cs =
  narrow cd i (matchInteger rules l cs) (matchInteger rules r cs)
matchInteger rules (Choices_BinInt cd i xs) cs =
  narrows cs cd i (\x -> matchInteger rules x cs) xs
matchInteger _     (Fail_BinInt cd info) _     = 
  failCons cd info
matchInteger rules (Guard_BinInt cd c int)  cs =
  guardCons cd c (matchInteger rules int $! addCs c cs)

matchNat :: NonDet a => [(Int, a)] -> Nat -> ConstStore -> a
matchNat []    _  _                    = failCons 0 defFailInfo
matchNat rules IHi _                   = maybe (failCons 0 defFailInfo) id $ lookup 1 rules
matchNat rules (O nat) cs              = 
  matchNat (map halfKey $ filter (evenPos.fst) rules) nat cs
  where
  evenPos n = even n && (0 < n)
matchNat rules (I nat) cs               = matchNat (map halfKey $ filter (odd.fst) rules) nat cs
matchNat rules (Choice_Nat cd i l r) cs = narrow cd i (matchNat rules l cs) (matchNat rules r cs)
matchNat rules (Choices_Nat cd i xs) cs = narrows cs cd i (\x -> matchNat rules x cs) xs
matchNat _     (Fail_Nat cd info) _     = failCons cd info
matchNat rules (Guard_Nat cd c nat) cs  = guardCons cd c $ matchNat rules nat $! addCs c cs

halfKey :: (Int,a) -> (Int,a)
halfKey =  mapFst (`div` 2)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

(&) :: C_Success -> C_Success -> ConstStore -> C_Success
(&) C_Success                   s _  = s
(&) x@(Fail_C_Success _ _)      _ _  = x
(&) (Guard_C_Success cd c e)    s cs = Guard_C_Success   cd c ((e & s) $! addCs c cs)
(&) (Choice_C_Success cd i a b) s cs = Choice_C_Success  cd i ((a & s) cs) ((b & s) cs)
(&) (Choices_C_Success cd i xs) s cs = 
      Choices_C_Success cd (narrowID i) (map (\x -> (x & s) cs) xs)

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
