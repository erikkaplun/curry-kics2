module StyledText (

  bold, underline, 
  black, blue, cyan, green, magenta, red, white, yellow,
  bgBlack, bgBlue, bgCyan, bgGreen, bgMagenta, bgRed, bgWhite, bgYellow,

  ShowS, boldS, underlineS,
  blackS, blueS, cyanS, greenS, magentaS, redS, whiteS, yellowS,
  bgBlackS, bgBlueS, bgCyanS, bgGreenS, bgMagentaS, bgRedS, bgWhiteS, 
  bgYellowS,

  Doc, boldDoc, underlineDoc, 
  blackDoc, blueDoc, cyanDoc, greenDoc, magentaDoc, redDoc, whiteDoc, 
  yellowDoc,
  bgBlackDoc, bgBlueDoc, bgCyanDoc, bgGreenDoc, bgMagentaDoc, bgRedDoc,
  bgWhiteDoc, bgYellowDoc,

  plainText, printStyledText {-setStyledText, appendStyledText,-} --styledHtml

  ) where

import Char
import Pretty
import AnsiCodes as Ansi
-- import GUI
--import HTML

boldChar      = chr 5
underlineChar = chr 6
endChar       = chr 7
blackChar     = chr 16
blueChar      = chr 17
cyanChar      = chr 18
greenChar     = chr 19
magentaChar   = chr 20
redChar       = chr 21
whiteChar     = chr 22
yellowChar    = chr 23
bgBlackChar   = chr 24
bgBlueChar    = chr 25
bgCyanChar    = chr 26
bgGreenChar   = chr 27
bgMagentaChar = chr 28
bgRedChar     = chr 29
bgWhiteChar   = chr 30
bgYellowChar  = chr 31

bold, underline, 
 black, blue, cyan, green, magenta, red, white, yellow,
 bgBlack, bgBlue, bgCyan, bgGreen, bgMagenta, bgRed, bgWhite, bgYellow
 :: String -> String
bold      = encl boldChar
underline = encl underlineChar
black     = encl blackChar
blue      = encl blueChar
cyan      = encl cyanChar
green     = encl greenChar
magenta   = encl magentaChar
red       = encl redChar
white     = encl whiteChar
yellow    = encl yellowChar
bgBlack   = encl bgBlackChar
bgBlue    = encl bgBlueChar
bgCyan    = encl bgCyanChar
bgGreen   = encl bgGreenChar
bgMagenta = encl bgMagentaChar
bgRed     = encl bgRedChar
bgWhite   = encl bgWhiteChar
bgYellow  = encl bgYellowChar

encl :: Char -> String -> String
encl c s = c : s ++ [endChar]


type ShowS = String -> String

boldS, underlineS,
 blackS, blueS, cyanS, greenS, magentaS, redS, whiteS, yellowS,
 bgBlackS, bgBlueS, bgCyanS, bgGreenS, bgMagentaS, bgRedS, bgWhiteS, bgYellowS
 :: ShowS -> ShowS
boldS      = enclS boldChar
underlineS = enclS underlineChar
blackS     = enclS blackChar
blueS      = enclS blueChar
cyanS      = enclS cyanChar
greenS     = enclS greenChar
magentaS   = enclS magentaChar
redS       = enclS redChar
whiteS     = enclS whiteChar
yellowS    = enclS yellowChar
bgBlackS   = enclS bgBlackChar
bgBlueS    = enclS bgBlueChar
bgCyanS    = enclS bgCyanChar
bgGreenS   = enclS bgGreenChar
bgMagentaS = enclS bgMagentaChar
bgRedS     = enclS bgRedChar
bgWhiteS   = enclS bgWhiteChar
bgYellowS  = enclS bgYellowChar

enclS :: Char -> ShowS -> ShowS
enclS c s = (c:) . s . (endChar:)


boldDoc, underlineDoc, 
 blackDoc, blueDoc, cyanDoc, greenDoc, magentaDoc, redDoc, whiteDoc, yellowDoc,
 bgBlackDoc, bgBlueDoc, bgCyanDoc, bgGreenDoc, bgMagentaDoc, bgRedDoc
 :: Doc -> Doc
boldDoc      = enclDoc boldChar
underlineDoc = enclDoc underlineChar
blackDoc     = enclDoc blackChar
blueDoc      = enclDoc blueChar
cyanDoc      = enclDoc cyanChar
greenDoc     = enclDoc greenChar
magentaDoc   = enclDoc magentaChar
redDoc       = enclDoc redChar
whiteDoc     = enclDoc whiteChar
yellowDoc    = enclDoc yellowChar
bgBlackDoc   = enclDoc bgBlackChar
bgBlueDoc    = enclDoc bgBlueChar
bgCyanDoc    = enclDoc bgCyanChar
bgGreenDoc   = enclDoc bgGreenChar
bgMagentaDoc = enclDoc bgMagentaChar
bgRedDoc     = enclDoc bgRedChar
bgWhiteDoc   = enclDoc bgWhiteChar
bgYellowDoc  = enclDoc bgYellowChar

enclDoc :: Char -> Doc -> Doc
enclDoc c = enclose (char c) (char endChar)


plainText :: String -> String
plainText = filter (not . special)

special :: Char -> Bool
special n = ord n `elem` ([5,6,7]++[16..31])


interpret
 :: [b] -> (a -> a -> a) -> (b -> b -> b) -> (b -> String -> a) -> [(b,b,b)]
 -> String -> a
interpret convs@(b:ul:cs) combine cst ast (t@(st,fg,bg):fs) s
  | null ys   = f xs
  | code == 5 = combine (f xs) $ -- bold
                 interpret convs combine cst ast ((b,fg,bg):t:fs) (tail ys)
  | code == 6 = combine (f xs) $ -- underline
                 interpret convs combine cst ast ((ul,fg,bg):t:fs) (tail ys)
  | code == 7 = combine (f xs) $ -- end
                 interpret convs combine cst ast fs (tail ys)
  | code < 24 = combine (f xs) $ -- foreground
                 interpret convs combine cst ast ((st,g,bg):t:fs) (tail ys)
  | otherwise = combine (f xs) $ -- background
                 interpret convs combine cst ast ((st,fg,g):t:fs) (tail ys)
 where
  f = ast $ cst st (cst fg bg)
  (xs,ys) = break special s
  code = ord (head ys)
  g = cs !! (code-16)


printStyledText :: String -> IO ()
printStyledText = putStrLn . toAnsiString

toAnsiString :: String -> String
toAnsiString
  = interpret 
     [Ansi.bold, Ansi.underline
     ,Ansi.black, Ansi.blue, Ansi.cyan, Ansi.green, Ansi.magenta, Ansi.red
     ,Ansi.white, Ansi.yellow
     ,Ansi.bgBlack, Ansi.bgBlue, Ansi.bgCyan, Ansi.bgGreen, Ansi.bgMagenta
     ,Ansi.bgRed, Ansi.bgWhite, Ansi.bgYellow]
     (++) (.) id [(id,id,id)]


-- setStyledText :: WidgetRef -> String -> GuiPort -> IO ()
-- setStyledText wref s gp = do
--   setValue wref "" gp
--   appendStyledText wref s gp

-- appendStyledText :: WidgetRef -> String -> GuiPort -> IO ()
-- appendStyledText wref s gp
--   = interpret styles (>>) (++) append [([],[],[])] s
--  where
--   append [] cs = appendValue wref cs gp
--   append st@(_:_) cs = appendStyledValue wref cs st gp

--   styles = [[Bold],[Underline]] ++ 
--            map ((:[]) . Fg) colors ++ 
--            map ((:[]) . Bg) colors

--   colors = [Black,Blue,Turquoise,Green,Purple,Red,White,Gold]
{-
styledHtml :: String -> [HtmlExp]
styledHtml = interpret styles (++) combine span [("","","")]
 where
  combine x y = x ++ " " ++ y
  span style s
    | all isSpace style = [htxt s]
    | otherwise = [HtmlStruct "span" [("style",style)] [htxt s]]

  styles = ["font-weight: bold;","text-decoration: underline;"]
        ++ map (\c -> "color: " ++ c ++ ";") colors
        ++ map (\c -> "background-color: " ++ c ++ ";") colors

  colors = ["black","blue","cyan","green","magenta","red","white","yellow"]
-}
-- main = do
--   printStyledText text
--   writeFile "StyledText.html"
--     (showHtmlPage (HtmlPage "StyledText" [] [styledHtml text]))
--   runInitGUI "StyledText"
--     (col [TextEdit [WRef ref, Width 40, Height 40]]) init
--  where
--   ref free
--   init = setStyledText ref text
--   text = unlines $ map (uncurry ($))
--     [(bold,"bold"),(underline,"underline")
--     ,(black,"black"),(blue,"blue"),(cyan,"cyan"),(green,"green")
--     ,(magenta,"magenta"),(red,"red"),(white,"white"),(yellow,"yellow")
--     ,(bgBlack,"bgBlack"),(bgBlue,"bgBlue"),(bgCyan,"bgCyan"),(bgGreen,"bgGreen")
--     ,(bgMagenta,"bgMagenta"),(bgRed,"bgRed"),(bgWhite,"bgWhite")
--     ,(bgYellow,"bgYellow")]
