{-# LANGUAGE MagicHash #-}

module Basics
  ( module Basics
  , module ID
  , module PrimTypes
  , module Search
  , module Types
  ) where

import Data.Char(ord)
import GHC.Exts (Int#, Char#, chr#)

import ID
import PrimTypes
import Search
import Types

nonAsciiChr :: Int# -> Char#
nonAsciiChr i = chr# i

-- Apply a function to the head normal form
d_dollar_bang :: (NonDet a, NonDet b) => (a -> b) -> a -> b
d_dollar_bang f x = match hnfChoice hnfNarrowed hnfFree failCons hnfGuard f x
  where
   hnfChoice i a b  = choiceCons i (d_dollar_bang f a) (d_dollar_bang f b)
   hnfNarrowed i xs = choicesCons i (map (d_dollar_bang f) xs)
   hnfFree i xs     = f (choicesCons i xs)
   hnfGuard c e     = guardCons c (d_dollar_bang f e)

-- Apply a non-deterministic function to the head normal form
nd_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> b
nd_dollar_bang f x s = match hnfChoice hnfNarrowed hnfFree failCons hnfGuard hnfVal x
  where
   hnfVal v         = nd_apply f v s
   -- TODO Do we better use leftSupply and rightSupply?
   hnfChoice i a b  = choiceCons i (nd_dollar_bang f a s) (nd_dollar_bang f b s)
   hnfNarrowed i xs = choicesCons i (map (\x -> nd_dollar_bang f x s) xs)
   hnfFree i xs     = nd_apply f (choicesCons i xs) s
   hnfGuard c e     = guardCons c (nd_dollar_bang f e s)

-- ---------------------------------------------------------------------------
-- Matching for Integers
-- ---------------------------------------------------------------------------

-- TODO: use unboxed int

matchInteger :: NonDet a => [(Int, a)] -> BinInt -> a
matchInteger rules (Neg nat)              =
  matchNat (map (mapFst abs) $ filter ((<0).fst) rules) nat
matchInteger rules Zero                   = maybe failCons id $ lookup 0 rules
matchInteger rules (Pos nat)              = matchNat (filter ((>0).fst) rules) nat
matchInteger rules (Choice_BinInt i l r) =
  narrow i (matchInteger rules l) (matchInteger rules r)
matchInteger rules (Choices_BinInt i cs) =
  narrows i $ map (matchInteger rules) cs
matchInteger _     Fail_BinInt           = failCons
matchInteger rules (Guard_BinInt cs int) = guardCons cs (matchInteger rules int)

matchNat :: NonDet a => [(Int, a)] -> Nat -> a
matchNat []    _                  = failCons
matchNat rules IHi                = maybe failCons id $ lookup 1 rules
matchNat rules (O nat)            = matchNat (map halfKey $ filter (evenPos.fst) rules) nat
  where
   evenPos n = even n && (0 < n)
matchNat rules (I nat)            = matchNat (map halfKey $ filter (odd.fst) rules) nat
matchNat rules (Choice_Nat i l r) = narrow i (matchNat rules l) (matchNat rules r)
matchNat rules (Choices_Nat i cs) = narrows i $ map (matchNat rules) cs
matchNat _     Fail_Nat           = failCons
matchNat rules (Guard_Nat cs nat) = guardCons cs $ matchNat rules nat


halfKey :: (Int,a) -> (Int,a)
halfKey =  mapFst (`div` 2)

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (a,b) = (f a,b)

-- ---------------------------------------------------------------------------
-- Matching for Chars
-- ---------------------------------------------------------------------------

matchChar :: NonDet a => [(Char,a)] -> BinInt -> a
matchChar rules = matchInteger (map (mapFst ord) rules)


(&) :: C_Success -> C_Success -> C_Success
(&) C_Success                s = s
(&) x@Fail_C_Success         _ = x
(&) (Guard_C_Success cs e)   s = Guard_C_Success cs (e & s)
(&) (Choice_C_Success i a b) s = Choice_C_Success i (a & s) (b & s)
(&) (Choices_C_Success i xs) s = Choices_C_Success (narrowID i) (map (& s) xs)

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
fromHaskellIO0 :: (ConvertCurryHaskell ca ha) => IO ha -> C_IO ca
fromHaskellIO0 hact = fromIO (hact >>= return . toCurry)

fromHaskellIO1 :: (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb) =>
                  (ha -> IO hb) -> ca -> C_IO cb
fromHaskellIO1 hact ca = fromIO (hact (fromCurry ca) >>= return . toCurry)

fromHaskellIO2 :: (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb,
                   ConvertCurryHaskell cc hc) =>
                  (ha -> hb -> IO hc) -> ca -> cb -> C_IO cc
fromHaskellIO2 hact ca cb =
  fromIO (hact (fromCurry ca) (fromCurry cb) >>= return . toCurry)

fromHaskellIO3 :: (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb,
                   ConvertCurryHaskell cc hc, ConvertCurryHaskell cd hd) =>
                  (ha -> hb -> hc -> IO hd) -> ca -> cb -> cc -> C_IO cd
fromHaskellIO3 hact ca cb cc =
 fromIO (hact (fromCurry ca) (fromCurry cb) (fromCurry cc) >>= return . toCurry)

-- ---------------------------------------------------------------------------
-- Auxiliaries for non-determinism
-- ---------------------------------------------------------------------------

-- make a deterministic function non-deterministic
nd :: (a -> b) -> a -> IDSupply -> b
nd f a _ = f a

wrapDX :: (c -> b) -> (a -> c) -> Func a b
wrapDX wrap f = wrapNX wrap (nd f)

wrapNX :: (c -> b) -> (a -> IDSupply -> c) -> Func a b
wrapNX wrap f = Func (\a s -> wrap $ f a s)

d_apply :: (a -> b) -> a -> b
d_apply f a = f a

nd_apply :: NonDet b => Func a b -> a -> IDSupply -> b
nd_apply fun a s = apply `d_dollar_bang` fun
  where apply (Func f) = f a s
        apply _        = error "Basics.nd_apply.apply: no ground term"
