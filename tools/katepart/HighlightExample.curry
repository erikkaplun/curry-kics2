{- This is a multiline comment,
   spanning two lines.
-}

-- This is a single line comment.

{-# NoImplicitPrelude, FunctionalPatterns #-}
module SyntaxHighlighting
  ( module SyntaxHighlighting
  , Data (..), Type, function, (</>) -- operator
  ) where

import qualified Maybe as Maybe hiding
  ( fromJust
  , fromMaybe
  )

import missspelled

--- Currydoc info
--- with second line.
type Type = ()

data Data
  = Cons1 Bool
  | Cons2 (() -> ())

type Record =
  { field1 :: Bool
  , field2 :: Int
  }

function, special :: Int -> Int
function _ = id local
  where local = 42
special _ = sum [1 .. 2]
  where hiding    = 1
        as        = 2
        qualified = 3

-- Literals
octal   = 0o123
dec     = 123
hex     = 0x123
float   = 2.0
char    = 'a'
charEsc = '\n'

string    = "A string"
stringGap = "A \  \gap"
stringNewlineGap = "A string \
                 \spanning two lines"
stringEsc = "Various\123\"\&\x123\DEL\^@ escape sequences"

-- Errors in literals
charTooShort = ''
charTooLong  = 'way too long'
charNewline  = '
a'
stringUnknownEscape = "\FOO"
stringNewline       = "
a"

tickChar = '''
