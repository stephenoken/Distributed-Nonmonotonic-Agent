% CGI.lhs
% This file was produced from CGI.lit

% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2007, 2008,  Andrew Rock
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

\module{CGI Programming} %%%%%%%%%%%%

Module \highlighttt{ABR.CGI} implements support for
Common Gateway Interface (CGI) programming.

\begin{code}
module ABR.CGI (
      mimeHeader, printMimeHeader, put, put', HTag,
      HAttributes, baseE_, isindexE_, linkE_,
      metaE_, nextidE_, inputE_, hrE_, brE_, imgE_,
      isindexN_, hrN_, brN_, htmlE, headE, titleE,
      bodyE, addressE, blockquoteE, formE, selectE,
      optionE, dlE, dtE, ddE, olE, ulE, dirE,
      menuE, liE, pE, preE, aE, mapE, areaE, citeE,
      codeE, emE, kbdE, sampE, strongE, varE, bE,
      iE, ttE, uE, tableE, captionE, trE, thE, tdE,
      divE, subE, supE, centerE, fontE, smallE,
      bigE, textareaE, h1E, h2E, h3E, h4E, h5E,
      h6E, htmlN, headN, titleN, bodyN, addressN,
      blockquoteN, dlN, dtN, ddN, olN, ulN, dirN,
      menuN, liN, pN, preN, citeN, codeN, emN,
      kbdN, sampN, strongN, varN, bN, iN, ttN, uN,
      tableN, captionN, trN, thN, tdN, subN, supN,
      centerN, smallN, bigN, h1N, h2N, h3N, h4N,
      h5N, h6N, htmlError, getQUERY_STRING,
      getPATH_INFO, getSCRIPT_NAME,
      getScriptDirectory, getCONTENT_LENGTH,
      getFormData, getFormData'
   ) where
\end{code}

\begin{code}
import Data.Char; import System.Environment; import Numeric
import Data.List; import Control.Exception
\end{code}

\begin{code}
import ABR.Data.BSTree; import ABR.List
import ABR.Text.Markup
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Requires review.


\submodule{Mime header} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

First things first. A CGI tool should print the
magic lines identifying the output as HTML.
\highlighttt{mimeHeader} is the MIME header text,
which is printed by \highlighttt{printMimeHeader}.

\begin{code}
mimeHeader :: String
mimeHeader = "Content-type: text/html\n\n"
\end{code}

\begin{code}
printMimeHeader :: IO ()
printMimeHeader = putStr mimeHeader
\end{code}

\submodule{Special character encoding} %%%%%%%%%%%%%%%%%%%

%\highlighttt{encodeHTML}~$c$ returns $c$'s
%special character encoding if $c \in \{$\verb"<",
%\verb">", \verb"&", \verb'"'$\}$, otherwise $c$.
%\highlighttt{encodeHTML'}~$c$ returns $c$'s
%special character encoding, additionally encoding
%all control characters.
%
%\begin{code}
%encodeHTML, encodeHTML' :: Char -> String
%[-CGI.tex]
%encodeHTML c = case c of
%   '<' -> "&lt;"
%   '>' -> "&gt;"
%   '&' -> "&amp;"
%   '"' -> "&quot;"
%   c   -> [c]
%encodeHTML' c = case c of
%   '<' -> "&lt;"
%   '>' -> "&gt;"
%   '&' -> "&amp;"
%   '"' -> "&quot;"
%   c   -> if isControl c then
%         "&#" ++ show (fromEnum c) ++ ";"
%      else
%         [c]
%[+CGI.tex]
%\end{code}
%
%\noindent \highlighttt{makeHTMLSafe}~$\mathit{cs}$ encodes
%all of the special characters in $\mathit{cs}$. It would be
%counter productive to put text containing tags
%through this filter.
%\highlighttt{makeHTMLSafe}~$\mathit{cs}$ encodes all of
%the special characters in $\mathit{cs}$ including all
%control characters.
%
%\begin{code}
%makeHTMLSafe, makeHTMLSafe' :: String -> String
%[-CGI.tex]
%makeHTMLSafe  = concatMap encodeHTML
%makeHTMLSafe' = concatMap encodeHTML'
%[+CGI.tex]
%\end{code}

\noindent \highlighttt{put}~$\mathit{cs}$ prints $\mathit{cs}$ with
all special characters encoded.
\highlighttt{put'}~$\mathit{cs}$ encodes all control
characters.

\begin{code}
put, put' :: String -> IO ()
put  = putStr . makeHTMLSafe
put' = putStr . makeHTMLSafe'
\end{code}

\submodule{HTML elements (generic)} %%%%%%%%%%%%%%%%%%%%%%

HTML elements have a name (a \highlighttt{HTag}).

\begin{code}
type HTag = String
\end{code}

\noindent HTML elements can have a list of
attributes (\highlighttt{HAttributes}) of the form
{\it name}{\tt =}{\it value}.

\begin{code}
type HAttributes = [(String,String)]
\end{code}

\noindent {\tt printAttributes}~$\mathit{atts}$
prints the attribute list $\mathit{atts}$, with spaces, {\tt
=}s and {\tt "}s as needed.

\begin{code}
printAttributes :: HAttributes -> IO ()
printAttributes as = putStr $
   unwords [a ++ "=\"" ++ v ++ "\"" | (a,v) <- as]
\end{code}

\noindent Some HTML elements are empty and do not
contain text.\\
{\tt emptyElement}~$\mathit{tag}~\mathit{attributes}$
prints a HTML element with the given $\mathit{tag}$ and
$\mathit{attributes}$.

\begin{code}
emptyElement :: HTag -> HAttributes -> IO ()
emptyElement tag attrs = do
   putChar '<'
   putStr tag
   case attrs of
      [] -> putChar '>'
      _  -> do putChar ' '
               printAttributes attrs
               putChar '>'
\end{code}

\noindent Most elements enclose other elements or
text.\\
{\tt element}~$\mathit{tag}~\mathit{attributes}~\mathit{contents}$
prints the $\mathit{tag}$, $\mathit{attributes}$, \\
whatever is printed
by $\mathit{contents}$ and then a closing tag.

\begin{code}
element :: HTag -> HAttributes -> IO a -> IO a
element tag attrs stuff = do
   emptyElement tag attrs
   returned <- stuff
   putStr "</"
   putStr tag
   putChar '>'
   return returned
\end{code}

\submodule{HTML elements (specific shortcuts)} %%%%%%%%%%%

This is not an exhaustive list. Add more as needed.

\highlightNoindex{$\mathit{tag}${\tt E\_}}~$\mathit{attributes}$ prints an
empty element its $\mathit{tag}$ and $\mathit{attributes}$.
\indextt{baseE\_}
\indextt{isindexE\_}
\indextt{linkE\_}
\indextt{metaE\_}
\indextt{nextidE\_}
\indextt{inputE\_}
\indextt{hrE\_}
\indextt{brE\_}
\indextt{imgE\_}

\begin{code}
baseE_, isindexE_, linkE_, metaE_, nextidE_,
   inputE_, hrE_, brE_, imgE_
   :: HAttributes -> IO ()
baseE_    = emptyElement "base"
isindexE_ = emptyElement "isindex"
linkE_    = emptyElement "link"
metaE_    = emptyElement "meta"
nextidE_  = emptyElement "nextid"
inputE_   = emptyElement "input"
hrE_      = emptyElement "hr"
brE_      = emptyElement "br"
imgE_     = emptyElement "img"
\end{code}

\noindent \highlightNoindex{$\mathit{tag}${\tt N\_}} prints an
empty element with its $\mathit{tag}$ and no attributes.
\indextt{isindexN\_}
\indextt{hrN\_}
\indextt{brN\_}

\begin{code}
isindexN_, hrN_, brN_
   :: IO ()
isindexN_ = isindexE_ []
hrN_      = hrE_      []
brN_      = brE_      []
\end{code}

\noindent \highlightNoindex{$\mathit{tag}${\tt E}}~$\mathit{attributes}~\mathit{contents}$
prints a non-empty element with its $\mathit{tag}$,
$\mathit{attributes}$ and $\mathit{contents}$.
\indextt{htmlE}
\indextt{headE}
\indextt{titleE}
\indextt{bodyE}
\indextt{addressE}
\indextt{blockquoteE}
\indextt{formE}
\indextt{selectE}
\indextt{optionE}
\indextt{dlE}
\indextt{dtE}
\indextt{ddE}
\indextt{olE}
\indextt{ulE}
\indextt{dirE}
\indextt{menuE}
\indextt{liE}
\indextt{pE}
\indextt{preE}
\indextt{aE}
\indextt{mapE}
\indextt{areaE}
\indextt{citeE}
\indextt{codeE}
\indextt{emE}
\indextt{kbdE}
\indextt{sampE}
\indextt{strongE}
\indextt{varE}
\indextt{bE}
\indextt{iE}
\indextt{ttE}
\indextt{uE}
\indextt{tableE}
\indextt{captionE}
\indextt{trE}
\indextt{thE}
\indextt{tdE}
\indextt{divE}
\indextt{subE}
\indextt{supE}
\indextt{centerE}
\indextt{fontE}
\indextt{smallE}
\indextt{bigE}
\indextt{textareaE}
\indextt{h1E}
\indextt{h2E}
\indextt{h3E}
\indextt{h4E}
\indextt{h5E}
\indextt{h6E}

\begin{code}
htmlE, headE, titleE, bodyE, addressE, blockquoteE,
   formE, selectE, optionE, dlE, dtE, ddE, olE,
   ulE, dirE, menuE, liE, pE, preE, aE, mapE,
   areaE, citeE, codeE, emE, kbdE, sampE, strongE,
   varE, bE, iE, ttE, uE, tableE, captionE, trE,
   thE, tdE, divE, subE, supE, centerE, fontE,
   smallE, bigE, textareaE, h1E, h2E, h3E, h4E,
   h5E, h6E
   :: HAttributes -> IO () -> IO ()
htmlE       = element "html"
headE       = element "head"
titleE      = element "title"
bodyE       = element "body"
addressE    = element "address"
blockquoteE = element "blockquote"
formE       = element "form"
selectE     = element "select"
optionE     = element "option"
dlE         = element "dl"
dtE         = element "dt"
ddE         = element "dd"
olE         = element "ol"
ulE         = element "ul"
dirE        = element "dir"
menuE       = element "menu"
liE         = element "li"
pE          = element "p"
preE        = element "pre"
aE          = element "a"
mapE        = element "map"
areaE       = element "area"
citeE       = element "cite"
codeE       = element "code"
emE         = element "em"
kbdE        = element "kbd"
sampE       = element "samp"
strongE     = element "strong"
varE        = element "var"
bE          = element "b"
iE          = element "i"
ttE         = element "tt"
uE          = element "u"
tableE      = element "table"
captionE    = element "caption"
trE         = element "tr"
thE         = element "th"
tdE         = element "td"
divE        = element "div"
subE        = element "sub"
supE        = element "sup"
centerE     = element "center"
fontE       = element "font"
smallE      = element "small"
bigE        = element "big"
textareaE   = element "textarea"
h1E         = element "h1"
h2E         = element "h2"
h3E         = element "h3"
h4E         = element "h4"
h5E         = element "h5"
h6E         = element "h6"
\end{code}

\noindent \highlightNoindex{$\mathit{tag}${\tt N}}~$\mathit{contents}$
prints a non-empty element with its $\mathit{tag}$,
$\mathit{contents}$ and no attributes.
\indextt{htmlN}
\indextt{headN}
\indextt{titleN}
\indextt{bodyN}
\indextt{addressN}
\indextt{blockquoteN}
\indextt{dlN}
\indextt{dtN}
\indextt{ddN}
\indextt{olN}
\indextt{ulN}
\indextt{dirN}
\indextt{menuN}
\indextt{liN}
\indextt{pN}
\indextt{preN}
\indextt{citeN}
\indextt{codeN}
\indextt{emN}
\indextt{kbdN}
\indextt{sampN}
\indextt{strongN}
\indextt{varN}
\indextt{bN}
\indextt{iN}
\indextt{ttN}
\indextt{uN}
\indextt{tableN}
\indextt{captionN}
\indextt{trN}
\indextt{thN}
\indextt{tdN}
\indextt{subN}
\indextt{supN}
\indextt{centerN}
\indextt{smallN}
\indextt{bigN}
\indextt{h1N}
\indextt{h2N}
\indextt{h3N}
\indextt{h4N}
\indextt{h5N}
\indextt{h6N}

\begin{code}
htmlN, headN, titleN, bodyN, addressN, blockquoteN,
   dlN, dtN, ddN, olN, ulN, dirN, menuN, liN, pN,
   preN, citeN, codeN, emN, kbdN, sampN, strongN,
   varN, bN, iN, ttN, uN, tableN, captionN, trN,
   thN, tdN, subN, supN, centerN, smallN, bigN,
   h1N, h2N, h3N, h4N, h5N, h6N
   :: IO () -> IO ()
htmlN       = htmlE       []
headN       = headE       []
titleN      = titleE      []
bodyN       = bodyE       []
addressN    = addressE    []
blockquoteN = blockquoteE []
dlN         = dlE         []
dtN         = dtE         []
ddN         = ddE         []
olN         = olE         []
ulN         = ulE         []
dirN        = dirE        []
menuN       = menuE       []
liN         = liE         []
pN          = pE          []
preN        = preE        []
citeN       = citeE       []
codeN       = codeE       []
emN         = emE         []
kbdN        = kbdE        []
sampN       = sampE       []
strongN     = strongE     []
varN        = varE        []
bN          = bE          []
iN          = iE          []
ttN         = ttE         []
uN          = uE          []
tableN      = tableE      []
captionN    = captionE    []
trN         = trE         []
thN         = thE         []
tdN         = tdE         []
subN        = subE        []
supN        = supE        []
centerN     = centerE     []
smallN      = smallE      []
bigN        = bigE        []
h1N         = h1E         []
h2N         = h2E         []
h3N         = h3E         []
h4N         = h4E         []
h5N         = h5E         []
h6N         = h6E         []
\end{code}


\submodule{Bailing out} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

If the tool wants to exit, displaying only an error
message use this. \highlighttt{htmlError}~$\mathit{message}$
prints an error message in HTML code. Ensure
$\mathit{message}$ is plain text (no tags please).

\begin{code}
htmlError :: String -> IO ()
htmlError message = htmlN (do
   headN (titleN (putStr "ERROR"))
   bodyN (pN (put message))
  )
\end{code}

\submodule{CGI inputs} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt getEnvVar}~$\mathit{var}$ tries to get an
environment variable $\mathit{var}$, catching any
exceptions, and returning an empty string if not
found or empty.

\begin{code}
getEnvVar :: String -> IO String
getEnvVar name = (catch (getEnv name) (\(NoMethodError e) -> return []))
\end{code}

\noindent \highlighttt{getQUERY\_STRING} returns
the text after the {\tt ?} in a URL.\\
\highlighttt{getPATH\_INFO} returns the extra path
info after the name of the CGI tool.
\highlighttt{getSCRIPT\_NAME} returns the URL of
the CGI tool. \highlighttt{getScriptDirectory}
returns the URL of the directory the CGI binary is
in.

\begin{code}
getQUERY_STRING, getPATH_INFO, getSCRIPT_NAME,
   getScriptDirectory :: IO String
getQUERY_STRING = getEnvVar "QUERY_STRING"
getPATH_INFO = getEnvVar "PATH_INFO"
getSCRIPT_NAME = getEnvVar "SCRIPT_NAME"
getScriptDirectory  = do
   script <- getSCRIPT_NAME
   return ((reverse . (dropWhile (/= '/')) . reverse)
           script)
\end{code}

\noindent \highlighttt{getCONTENT\_LENGTH} returns
the number of bytes of content arriving via
standard input.

\begin{code}
getCONTENT_LENGTH :: IO Int
getCONTENT_LENGTH = do
   contentLength <- getEnvVar "CONTENT_LENGTH"
   return (if contentLength == []
           then 0
           else read contentLength)
\end{code}

\noindent \highlighttt{getFormData} reads standard
input to obtain the post method inputs, decodes
them and returns them in a binary search tree.

\begin{code}
getFormData :: IO (BSTree String String)
getFormData = do
      contentLength <- getCONTENT_LENGTH
      cs <- getContents
      return $ pairs2BST $ decodeForm $
         take contentLength cs
   where
   decodeForm :: String -> [(String,String)]
   decodeForm = map nv . chop '&'
   nv :: String -> (String, String)
   nv cs =
      let [name,value] = case chop '=' cs of
           [n,v] -> [n,v]
           [n] -> [n, ""]
           _     -> error $ "Can't decode: " ++
                       show cs
      in (decode name, decode value)
   decode :: String -> String
   decode cs = case cs of
      []                 ->
         []
      '+' : xs           ->
         ' ' : decode xs
      '%' : x1 : x2 : xs ->
         toEnum ((fst . head . readHex) [x1,x2])
            : decode xs
      x : xs             ->
         x : decode xs
\end{code}

\noindent \highlighttt{getFormData'} reads standard input to
obtain the post method inputs in the
\verb+enctype="multipart/form-data"+ format, decodes them and
returns them in a binary search tree.\footnote{This function in part by Annie Lo,
2134CIT Programming Paradigms and Languages, Advanced Studies
project, 2004.}

\begin{code}
getFormData' :: IO (BSTree String String)
getFormData' = do
       contentLength <- getCONTENT_LENGTH
       cs <- getContents
       --preN $ put cs
       return $ pairs2BST $ decodeForm $ take contentLength cs
   where
   decodeForm :: String -> [(String, String)]
   decodeForm cs =
      let cs' = dropWhile (/= '-') cs
          (bs, cs'') = break isControl cs'
          css = init (chops ("\r\n" ++ bs) cs'')
      in concatMap pairs css
   pairs :: String -> [(String,String)]
   pairs cs =
      let cs' = dropWhile (/= ';') cs
          '"' : cs'' = dropWhile (/= '"') cs'
          (name,cs''') = break (== '"') cs''
          fn = "\"; filename=\""
      in if fn `isPrefixOf` cs''' then
            let ds = drop (length fn) cs'''
                (filename,ds') = break (== '"') ds
                ds'' = drop 2 (dropWhile (/= ':') ds')
                (mimetype,ds''') = break isSpace ds''
            in [(name, drop 4 ds'''),
                (name ++ ".filename", filename),
                (name ++ ".type", mimetype)]
         else
            [(name, drop 5 cs''')]
\end{code}
