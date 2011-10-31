------------------------------------------------------------------------------
--- Library to translate simple markdown documents into HTML.
---
--- A simple markdown document might contain the following elements
--- that are considered by this translator:
--- *emph* _emph_ **strong** __strong__ `code`
--- Backslash escapes in the form of `\\c` are recognized as character `c`.
--- 
--- @author Michael Hanus
--- @version October 2011
------------------------------------------------------------------------------

module Markdown(markdownText2HTML,markdownText2LaTeX)
 where

import List
import HTML
import HtmlParser

-----------------------------------------------------------------------
-- Structure of markdown documents
type MarkdownDoc = [MarkdownElem]

data MarkdownElem = MDText String
                  | MDEmph String
                  | MDStrong String
                  | MDCode String

-----------------------------------------------------------------------
-- Parse markdown document from textual representation.

fromMarkdownText :: String -> MarkdownDoc
fromMarkdownText = outsideMarkdownElem []

outsideMarkdownElem :: String -> String -> MarkdownDoc
outsideMarkdownElem txt s = case s of
  [] -> if null txt then [] else [MDText (reverse txt)]
  ('\\':c:cs) -> outsideMarkdownElem (c:txt) cs
  ('*':'*':cs) -> addPrevious txt $ insideMarkdownElem "**" [] cs
  ('_':'_':cs) -> addPrevious txt $ insideMarkdownElem "__" [] cs
  ('*':cs)     -> addPrevious txt $ insideMarkdownElem "*" [] cs
  ('_':cs)     -> addPrevious txt $ insideMarkdownElem "_" [] cs
  ('`':cs)     -> addPrevious txt $ insideMarkdownElem "`" [] cs
  (c:cs)   -> outsideMarkdownElem (c:txt) cs
 where
  addPrevious ptxt xs = if null ptxt then xs else MDText (reverse ptxt) : xs

insideMarkdownElem marker etext s =
  if marker `isPrefixOf` s
  then text2MDElem marker (reverse etext)
        : fromMarkdownText (drop (length marker) s)
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

-----------------------------------------------------------------------
-- Translate markdown document to HTML.

mdDoc2html :: MarkdownDoc -> [HtmlExp]
mdDoc2html = map mdElem2html

mdElem2html :: MarkdownElem -> HtmlExp
mdElem2html (MDText s) = HtmlText s
mdElem2html (MDEmph s) = emphasize [HtmlText s]
mdElem2html (MDStrong s) = HtmlStruct "strong" [] [HtmlText s]
mdElem2html (MDCode s) = code [HtmlText s]

m2 = showHtmlExps (mdDoc2html m1)


markdownText2HTML :: String -> [HtmlExp]
markdownText2HTML = mdDoc2html . fromMarkdownText

-----------------------------------------------------------------------
-- Translate markdown document to LaTeX string.

mdDoc2latex :: MarkdownDoc -> String
mdDoc2latex = concatMap mdElem2latex

mdElem2latex :: MarkdownElem -> String
mdElem2latex (MDText s) = html2latex s
mdElem2latex (MDEmph s) = "\\emph{"++html2latex s++"}"
mdElem2latex (MDStrong s) = "\\textbf{"++html2latex s++"}"
mdElem2latex (MDCode s) = "\\texttt{"++html2latex s++"}"

html2latex = showLatexExps . parseHtmlString

m3 = mdDoc2latex m1

markdownText2LaTeX :: String -> String
markdownText2LaTeX = mdDoc2latex . fromMarkdownText

-----------------------------------------------------------------------