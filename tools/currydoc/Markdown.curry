------------------------------------------------------------------------------
--- Library to translate simple markdown documents into HTML.
---
--- A simple markdown document might contain the following elements
--- that are considered by this translator:
--- *emph* _emph_ **strong** __strong__ `code`
--- Backslash escapes in the form of `\\c` are recognized as character `c`.
--- 
--- @author Michael Hanus
--- @version November 2011
------------------------------------------------------------------------------

module Markdown(markdownText2HTML,markdownText2LaTeX)
 where

import List
import Char
import HTML
import HtmlParser

-----------------------------------------------------------------------
-- Structure of markdown documents
type MarkdownDoc = [MarkdownElem]

data MarkdownElem = MDText String
                  | MDEmph String
                  | MDStrong String
                  | MDCode String
                  | MDHRef String String
                  | MDPar MarkdownDoc
                  | MDCodeBlock String
                  | MDUItem String -- only for internal use
                  | MDOItem String -- only for internal use
                  | MDUList [MarkdownDoc]
                  | MDOList [MarkdownDoc]
                  | MDQuote MarkdownDoc
                  | MDHRule
                  | MDHeader Int String

-----------------------------------------------------------------------
-- Parse markdown document from textual representation.

fromMarkdownText :: String -> MarkdownDoc
fromMarkdownText = groupMarkDownElems . markdownText

-- Group adjacent markdown elements together.
groupMarkDownElems mes = case mes of
  [] -> []
  (MDUItem itxt : mds) -> joinUItems [itxt] mds
  (MDOItem itxt : mds) -> joinOItems [itxt] mds
  --(MDCodeBlock cl : mds) -> joinCodeLines cl mds
  (md:mds) -> md : groupMarkDownElems mds

joinUItems items mes = case mes of
  [] -> [MDUList (reverse (map fromMarkdownText items))]
  (MDUItem itxt : mds) -> joinUItems (itxt:items) mds
  (md:mds) -> MDUList (reverse (map fromMarkdownText items))
              : groupMarkDownElems (md:mds)

joinOItems items mes = case mes of
  [] -> [MDOList (reverse (map fromMarkdownText items))]
  (MDOItem itxt : mds) -> joinOItems (itxt:items) mds
  (md:mds) -> MDOList (reverse (map fromMarkdownText items))
              : groupMarkDownElems (md:mds)

-- Basic reader for markdown text.
markdownText :: String -> MarkdownDoc
markdownText [] = []
markdownText txt@(_:_) = markdownLine (break (=='\n') txt)

-- Analyze the first line of a markdown text:
markdownLine (fstline,remtxt)
 | all isSpace fstline
  = markdownText (dropFirst remtxt)
 | take 1 fstline == "#"
  = tryMDHeader fstline (dropFirst remtxt)
 | isHRule fstline
  = MDHRule : markdownText (dropFirst remtxt)
 | take 2 fstline == "> " -- start of a quoted text
  = markdownQuote (drop 2 fstline) (dropFirst remtxt)
 | take 4 fstline == "    " && (fstline!!4/=' ') -- four space indent for code
  = markdownCodeBlock (drop 4 fstline) (dropFirst remtxt)
 | take 4 fstline == "  * "  -- start of an unordered item
  = markdownItem MDUItem 4 (drop 4 fstline) (dropFirst remtxt)
 | take 3 fstline == " * "  -- start of an unordered item
  = markdownItem MDUItem 3 (drop 3 fstline) (dropFirst remtxt)
 | nitemlen > 0 -- start of a numbered item
  = markdownItem MDOItem nitemlen (drop nitemlen fstline) (dropFirst remtxt)
 | otherwise = markdownPar fstline (dropFirst remtxt)
 where
  nitemlen = isNumberedItemLine fstline

dropFirst s = if null s then [] else tail s

-- translate a header line
tryMDHeader s rtxt =
  let (sharps,htxt) = break (==' ') s
      level = length sharps
   in if null htxt || level>6
      then markdownPar s rtxt
      else MDHeader level (dropFirst htxt) : markdownText rtxt

-- is a line a horizontal rule:
isHRule l =
  (all (\c -> isSpace c || c=='-') l && length (filter (=='-') l) > 3) ||
  (all (\c -> isSpace c || c=='*') l && length (filter (=='*') l) > 3)

-- check whether a line starts with an indented number and return indent:
isNumberedItemLine s =
    if take 2 s == "  " && length s > 3
    then checkNumber 2 (drop 2 s)
    else if take 1 s == " " && length s > 2
         then checkNumber 1 (tail s)
         else 0
 where
  checkNumber indt numtxt =
    let (ns,_) = break (==' ') numtxt
        nsl = length ns
     in if nsl>0 && all isDigit (take (nsl-1) ns) && ns!!(nsl-1)=='.'
        then nsl+indt+1
        else 0

-- parse a paragraph:
markdownPar ptxt txt =
  if null txt || isSpace (head txt)
  then MDPar (outsideMarkdownElem "" ptxt) : markdownText txt
  else let (fstline,remtxt) = break (=='\n') txt
        in if null remtxt
           then [MDPar (outsideMarkdownElem "" (ptxt++'\n':fstline))]
           else markdownPar (ptxt++'\n':fstline) (tail remtxt)

-- parse a quoted section:
markdownQuote qtxt txt =
  if take 2 txt == "> "
  then let (fstline,remtxt) = break (=='\n') (drop 2 txt)
        in if null remtxt
           then [MDQuote (fromMarkdownText (qtxt++'\n':fstline))]
           else markdownQuote (qtxt++'\n':fstline) (tail remtxt)
  else MDQuote (fromMarkdownText qtxt) : markdownText txt

-- parse a program block:
markdownCodeBlock ctxt txt =
  if take 4 txt == "    "
  then let (fstline,remtxt) = break (=='\n') (drop 4 txt)
        in if null remtxt
           then [MDCodeBlock (ctxt++'\n':fstline)]
           else markdownCodeBlock (ctxt++'\n':fstline) (tail remtxt)
  else MDCodeBlock ctxt : markdownText txt

-- parse a markdown list item:
markdownItem icons n itxt txt =
  if take n txt == take n (repeat ' ')
  then let (fstline,remtxt) = break (=='\n') (drop n txt)
        in if null remtxt
           then [icons (itxt++'\n':fstline)]
           else markdownItem icons n (itxt++'\n':fstline) (tail remtxt)
  else let (fstline,remtxt) = break (=='\n') txt
        in if all isSpace fstline
           then if null remtxt
                then [icons itxt]
                else markdownItem icons n (itxt++"\n") (tail remtxt)
           else icons itxt : markdownText txt

-- Analyze markdown text outside an element like emphasis, code, strong:
outsideMarkdownElem :: String -> String -> MarkdownDoc
outsideMarkdownElem txt s = case s of
  [] -> addPrevious txt []
  ('\\':c:cs)  -> outsideMarkdownElem (c:txt) cs
  ('*':'*':cs) -> addPrevious txt $ insideMarkdownElem "**" [] cs
  ('_':'_':cs) -> addPrevious txt $ insideMarkdownElem "__" [] cs
  ('*':cs)     -> addPrevious txt $ insideMarkdownElem "*" [] cs
  ('_':cs)     -> addPrevious txt $ insideMarkdownElem "_" [] cs
  ('`':cs)     -> addPrevious txt $ insideMarkdownElem "`" [] cs
  ('[':cs) -> addPrevious txt $ tryParseLink cs
  ('<':cs) -> if take 4 cs == "http"
              then addPrevious txt $ markdownHRef cs
              else outsideMarkdownElem ('<':txt) cs
  (c:cs)   -> outsideMarkdownElem (c:txt) cs

addPrevious ptxt xs = if null ptxt then xs else MDText (reverse ptxt) : xs

-- Try to parse a link of the form [link test](url)
tryParseLink txt = let (linktxt,rtxt) = break (==']') txt in
  if null rtxt || null (tail rtxt) || (rtxt!!1 /= '(')
  then outsideMarkdownElem "[" txt
  else let (url,mtxt) = break (==')') (drop 2 rtxt)
        in if null mtxt
           then outsideMarkdownElem "[" txt
           else MDHRef linktxt url : outsideMarkdownElem "" (tail mtxt)

markdownHRef txt = let (url,rtxt) = break (=='>') txt in
  if null rtxt
  then outsideMarkdownElem "" ('<':txt)
  else MDHRef url url : outsideMarkdownElem "" (dropFirst rtxt)

insideMarkdownElem marker etext s =
  if marker `isPrefixOf` s
  then text2MDElem marker (reverse etext)
        : outsideMarkdownElem "" (drop (length marker) s)
  else case s of
        []     -> if null etext
                  then []
                  else  [text2MDElem marker (reverse etext)]
        ('\\':c:cs) -> insideMarkdownElem marker (c:etext) cs
        (c:cs)      -> insideMarkdownElem marker (c:etext) cs

text2MDElem marker txt = case marker of
  "**" -> MDStrong txt
  "__" -> MDStrong txt
  "*"  -> MDEmph txt
  "_"  -> MDEmph txt
  "`"  -> MDCode txt
  _    -> error $ "Markdown.text2MDElem: unknown marker \""++marker++"\""

m1 = fromMarkdownText
        "Try \\*emph\\* *emph* _emph_ **strong** __strong__ `c\\`o~de`"

m = readFile "test.txt" >>= print . fromMarkdownText

-----------------------------------------------------------------------
-- Translate markdown document to HTML.

mdDoc2html :: MarkdownDoc -> [HtmlExp]
mdDoc2html = map mdElem2html

mdElem2html :: MarkdownElem -> HtmlExp
mdElem2html (MDText s) = htxt s
mdElem2html (MDEmph s) = emphasize [htxt s]
mdElem2html (MDStrong s) = HtmlStruct "strong" [] [htxt s]
mdElem2html (MDHRef s url) = if s==url then href url [code [htxt s]]
                                       else href url [htxt s]
mdElem2html (MDCode s) = code [htxt s]
mdElem2html (MDCodeBlock s) = verbatim s
mdElem2html (MDQuote md) = HtmlStruct "blockquote" [] (mdDoc2html md)
mdElem2html (MDPar md) = par (mdDoc2html md)
mdElem2html (MDUList s) = ulist (map mdDoc2html s)
mdElem2html (MDOList s) = olist (map mdDoc2html s)
mdElem2html MDHRule = hrule
mdElem2html (MDHeader l s) = HtmlStruct ('h':show l) [] [htxt s]

m2 = showHtmlExps (mdDoc2html m1)


markdownText2HTML :: String -> [HtmlExp]
markdownText2HTML = mdDoc2html . fromMarkdownText

m2' = readFile "test.txt" >>= putStrLn . showHtmlExps . markdownText2HTML

-----------------------------------------------------------------------
-- Translate markdown document to LaTeX string.

mdDoc2latex :: MarkdownDoc -> String
mdDoc2latex = concatMap mdElem2latex

mdElem2latex :: MarkdownElem -> String
mdElem2latex (MDText s) = html2latex s
mdElem2latex (MDEmph s) = "\\emph{"++html2latex s++"}"
mdElem2latex (MDStrong s) = "\\textbf{"++html2latex s++"}"
mdElem2latex (MDHRef s url) = if s==url then "\\url{"++url++"}"
                                        else "\\href{"++url++"}{"++s++"}"
mdElem2latex (MDCode s) = "\\texttt{"++html2latex s++"}"
mdElem2latex (MDCodeBlock s) = "\\begin{verbatim}\n"++s++"\n\\end{verbatim}\n"
mdElem2latex (MDQuote md) = "\\begin{quote}\n"++mdDoc2latex md++"\\end{quote}\n"
mdElem2latex (MDPar md) = mdDoc2latex md++"\n\n" --"\\medskip\n"
mdElem2latex (MDUList s) = "\\begin{itemize}"++
                           concatMap (\i -> "\n\\item\n"++mdDoc2latex i) s ++
                           "\\end{itemize}\n"
mdElem2latex (MDOList s) = "\\begin{enumerate}"++
                           concatMap (\i -> "\n\\item\n"++mdDoc2latex i) s ++
                           "\\end{enumerate}\n"
mdElem2latex MDHRule = "\\begin{center}\\rule{3in}{0.4pt}\\end{center}\n\n"
mdElem2latex (MDHeader l s) = case l of
  1 -> "\\section{"++s++"}\n\n"
  2 -> "\\subsection{"++s++"}\n\n"
  3 -> "\\subsubsection{"++s++"}\n\n"
  4 -> "\\paragraph{"++s++"}\n\n"
  5 -> "\\textbf{"++s++"}\n\n"
  _ -> "\\emph{"++s++"}\n\n"


html2latex = showLatexExps . parseHtmlString

m3 = mdDoc2latex m1

markdownText2LaTeX :: String -> String
markdownText2LaTeX = mdDoc2latex . fromMarkdownText

m4 = readFile "test.txt" >>= putStrLn . markdownText2LaTeX

-----------------------------------------------------------------------
