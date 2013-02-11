{- This is a multiline comment,
   spanning two lines.
-}

{-# Pragma1,
    Pragma2
#-}

module SyntaxHighlighting
  ( module SyntaxHighlighting
  , Data (..), Type, function
  ) where

import qualified Maybe as Maybe hiding
  ( fromJust
  , fromMaybe
  )

import missspelling

-- This is a single line comment

--- Currydoc info
--- second line.
type Type = ()

data Data
  = Cons1
  | Cons2

type Record =
  { field1 :: Bool
  , field2 :: Int
  }

function :: Int -> Int
function _ = id local
  where local = 42

special = ()
  where hiding = 1 -- no keyword!
        as     = 2 -- no keyword!

char = 'a'

string = "A string"

octal = 0o123

hex = 0x42

enum = [1 .. 2]
