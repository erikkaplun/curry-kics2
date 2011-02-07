------------------------------------------------------------------------------
--- Library for formatted output on terminals
---
--- Information on ANSI Codes can be found at 
--- http://www.dee.ufcg.edu.br/~rrbrandt/tools/ansi.html
---
--- @author Sebastian Fischer
------------------------------------------------------------------------------

module AnsiCodes(

  -- exported functions for cursor movement
  cursorPos, cursorHome,
  cursorUp, cursorDown, cursorFwd, cursorBack,
  saveCursor, restoreCursor,

  -- exported functions for graphics control
  clear, eraseLine,

  -- exported functions for formatted output
  bold, underline, revColors, concealed,
  black, red, green, yellow, blue, cyan, magenta, white,
  bgBlack, bgRed, bgGreen, bgYellow, bgBlue, bgCyan, bgMagenta, bgWhite,

  -- exported functions for string operations
  ansiLength

  )  where

import Char
import List

-- escape character
esc = chr 27


--- The functions for cursor movement

cmd s = esc:"[" ++ s

--- move cursor to position
cursorPos r c = cmd (show r ++ ";" ++ show c ++ "H")

--- move cursor to home position
cursorHome = cmd "H"

moveCursor :: String -> String -> String
moveCursor s n = cmd (show n ++ s)

--- move cursor n lines up
cursorUp = moveCursor "A"

--- move cursor n lines down
cursorDown = moveCursor "B"

--- move cursor n columns forward
cursorFwd = moveCursor "C"

--- move cursor n columns backward
cursorBack = moveCursor "D"

--- save cursor position
saveCursor = cmd "s"

--- restore saved cursor position
restoreCursor = cmd "u"


--- The functions for controlling graphics

--- clear screen
clear = cmd "2J"

--- erase line
eraseLine = cmd "K"


mode n s = cmd (show n ++ "m" ++ s ++ if isSuffixOf end s then "" else end)
 where
  end = cmd "0m"

--isSuffixOf s l = [] /= findall (\p -> p ++ s =:= l)
isSuffixOf s = isPrefixOf (reverse s) . reverse


--- format text

bold      = mode 1
underline = mode 4
revColors = mode 7
concealed = mode 8
black     = mode 30
red       = mode 31
green     = mode 32
yellow    = mode 33
blue      = mode 34
magenta   = mode 35
cyan      = mode 36
white     = mode 37
bgBlack   = mode 40
bgRed     = mode 41
bgGreen   = mode 42
bgYellow  = mode 43
bgBlue    = mode 44
bgMagenta = mode 45
bgCyan    = mode 46
bgWhite   = mode 47


-- functions for string operations

ansiLength :: String -> Int
ansiLength s = aux s (length s)
 where
  aux [] n = n
  aux (c:cs) n | c==esc && isDigit (cs!!2)
               = aux (tail (tail (tail (tail cs)))) (n-5)
               | c==esc  =  aux (tail (tail (tail cs))) (n-4)
               | otherwise  = aux cs n

------------------------------------------------------------------------------

