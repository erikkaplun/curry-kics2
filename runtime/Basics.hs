{-# LANGUAGE MagicHash #-}

module Basics
  ( module Basics
  , module ConstStore
  , module ID
  , module PrimTypes
  , module Search
  , module Types
  ) where

import Data.Char(ord)
import GHC.Exts (Int#, Char#, chr#)

import ConstStore
import ID
import PrimTypes
import Search
import Types

-- ---------------------------------------------------------------------------
-- Auxiliaries for non-determinism and higher order
-- ---------------------------------------------------------------------------

-- |Make a deterministic function non-deterministic
nd :: (a -> ConstStore -> b) -> a -> IDSupply -> ConstStore -> b
nd f a _ cs = f a cs

-- |Make higher order functions take a constraint store after each argument
acceptCs ::  (b -> c)-> (a -> b) -> a -> ConstStore -> c
acceptCs cont f x _ = cont (f x)

wrapDX :: (c -> b) -> (a -> ConstStore -> c) -> Func a b
wrapDX wrap f = wrapNX wrap (nd f)

wrapNX :: (c -> b) -> (a -> IDSupply -> ConstStore -> c) -> Func a b
wrapNX wrap f = Func (\a s cs -> wrap $ f a s cs)

d_apply :: (a -> ConstStore -> b) -> a -> ConstStore -> b
d_apply f a cs = f a cs

nd_apply :: NonDet b => Func a b -> a -> IDSupply -> ConstStore -> b
nd_apply fun a s cs = d_dollar_bang apply fun cs
  where apply (Func f)  cs' = f a s cs'
        apply _           _ = error "Basics.nd_apply.apply: no ground term"

-- ---------------------------------------------------------------------------
-- Auxilaries for normalforms
-- ---------------------------------------------------------------------------

-- Apply a function to the head normal form
d_dollar_bang :: (NonDet a, NonDet b) => (a -> ConstStore -> b) -> a -> ConstStore -> b
d_dollar_bang f x cs = match hnfChoice hnfNarrowed hnfFree failCons hnfGuard (flip f cs) x
  where
   hnfChoice i a b  = choiceCons i (d_dollar_bang f a cs) (d_dollar_bang f b cs)
   hnfNarrowed i xs = choicesCons i (map (\y -> d_dollar_bang f y cs) xs)
   hnfFree i xs     = f (choicesCons i xs) cs
   hnfGuard c e     = guardCons c (d_dollar_bang f e $! addCs c cs)

-- Apply a non-deterministic function to the head normal form
nd_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> ConstStore -> b
nd_dollar_bang f x s cs = match hnfChoice hnfNarrowed hnfFree failCons hnfGuard hnfVal x
  where
   hnfVal v         = nd_apply f v s cs
   -- TODO Do we better use leftSupply and rightSupply?
   hnfChoice i a b  = choiceCons i (nd_dollar_bang f a s cs) (nd_dollar_bang f b s cs)
   hnfNarrowed i xs = choicesCons i (map (\y -> nd_dollar_bang f y s cs) xs)
   hnfFree i xs     = nd_apply f (choicesCons i xs) s cs
   hnfGuard c e     = guardCons c (nd_dollar_bang f e s $! addCs c cs)

-- ---------------------------------------------------------------------------
-- Pattern matching utilities for Literals
-- ---------------------------------------------------------------------------

nonAsciiChr :: Int# -> Char#
nonAsciiChr i = chr# i

matchChar :: NonDet a => [(Char,a)] -> BinInt -> ConstStore -> a
matchChar rules cs = matchInteger (map (mapFst ord) rules) cs

-- TODO@fre: use unboxed int

matchInteger :: NonDet a => [(Int, a)] -> BinInt -> ConstStore -> a
matchInteger rules (Neg nat) cs             =
  matchNat (map (mapFst abs) $ filter ((<0).fst) rules) nat cs
matchInteger rules Zero _                   = maybe failCons id $ lookup 0 rules
matchInteger rules (Pos nat) cs             = matchNat (filter ((>0).fst) rules) nat cs
matchInteger rules (Choice_BinInt i l r) cs =
  narrow i (matchInteger rules l cs) (matchInteger rules r cs)
matchInteger rules (Choices_BinInt i xs) cs =
  narrows cs i (\x -> matchInteger rules x cs) xs
matchInteger _     Fail_BinInt _            = failCons
matchInteger rules (Guard_BinInt c int)  cs = guardCons c (matchInteger rules int $! addCs c cs)

matchNat :: NonDet a => [(Int, a)] -> Nat -> ConstStore -> a
matchNat []    _  _                 = failCons
matchNat rules IHi _                = maybe failCons id $ lookup 1 rules
matchNat rules (O nat) cs           = matchNat (map halfKey $ filter (evenPos.fst) rules) nat cs
  where
  evenPos n = even n && (0 < n)
matchNat rules (I nat) cs            = matchNat (map halfKey $ filter (odd.fst) rules) nat cs
matchNat rules (Choice_Nat i l r) cs = narrow i (matchNat rules l cs) (matchNat rules r cs)
matchNat rules (Choices_Nat i xs) cs = narrows cs i (\x -> matchNat rules x cs) xs
matchNat _     Fail_Nat _            = failCons
matchNat rules (Guard_Nat c nat) cs  = guardCons c $ matchNat rules nat $! addCs c cs

halfKey :: (Int,a) -> (Int,a)
halfKey =  mapFst (`div` 2)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

(&) :: C_Success -> C_Success -> ConstStore -> C_Success
(&) C_Success                s _  = s
(&) x@Fail_C_Success         _ _  = x
(&) (Guard_C_Success c e)    s cs = Guard_C_Success   c ((e & s) $! addCs c cs)
(&) (Choice_C_Success i a b) s cs = Choice_C_Success  i ((a & s) cs) ((b & s) cs)
(&) (Choices_C_Success i xs) s cs = Choices_C_Success (narrowID i) (map (\x -> (x & s) cs) xs)

{- parallel & from Bernd
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

 -- Use a Haskell IO action to implement a Curry IO action:
fromHaskellIO0 :: ConvertCurryHaskell ca ha => IO ha -> C_IO ca
fromHaskellIO0 = toCurry
-- fromHaskellIO0 hact = fromIO (hact >>= return . toCurry)

fromHaskellIO1 :: (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb)
               => (ha -> IO hb) -> ca -> C_IO cb
fromHaskellIO1 = toCurry
-- fromHaskellIO1 hact ca = fromIO (hact (fromCurry ca) >>= return . toCurry)

fromHaskellIO2 :: (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb,
                   ConvertCurryHaskell cc hc)
               => (ha -> hb -> IO hc) -> ca -> cb -> C_IO cc
fromHaskellIO2 = toCurry
-- fromHaskellIO2 hact ca cb =
--   fromIO (hact (fromCurry ca) (fromCurry cb) >>= return . toCurry)

fromHaskellIO3 :: (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb,
                   ConvertCurryHaskell cc hc, ConvertCurryHaskell cd hd)
               => (ha -> hb -> hc -> IO hd) -> ca -> cb -> cc -> C_IO cd
fromHaskellIO3 = toCurry
-- fromHaskellIO3 hact ca cb cc =
--  fromIO (hact (fromCurry ca) (fromCurry cb) (fromCurry cc) >>= return . toCurry)
