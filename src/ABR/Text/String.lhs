% String.lhs
% This file was produced from String.lit

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

\module{Text.String: String Utilities} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Text.String} is a collection of functions
that operate on strings.

\begin{code}
module ABR.Text.String (
      wordWrap, lJustify, rJustify, lJustify', rJustify',
      justifyColumn, makeTable, spaceColumns, makeTableL,
      makeTableMR, fields, unfields, nameLT, trim,
      fixNewlines, fixNewlines', spaces, findClosest,
      (++/++), (++.++), catenateWith, substs, subst,
      subHashNums, isCardinal, isFixed, isFloat,
      isSignedCardinal, isSignedFixed, isSignedFloat, 
      unString, enString
   ) where
\end{code}

\begin{code}
import Data.List; import Data.Char; import Numeric
\end{code}

\begin{code}
import ABR.Control.Check; import ABR.Parser.Pos
import ABR.Parser; import ABR.Parser.Lexers
import ABR.Parser.Checks
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2009-04-13: Changed to {\tt ABR.\emph{Text}.String}.
   

\submodule{Word wrapping} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{wordWrap}~$\mathit{width}~\mathit{cs}$ wraps the words
in $\mathit{cs}$ to no wider than $\mathit{width}$, unless a word is
wider than $\mathit{width}$, returning a list of lines.

\begin{code}
wordWrap :: Int -> String -> [String]
wordWrap width = wrap 0 [] . words
   where
   wrap :: Int -> String -> [String] -> [String]
   wrap 0 [] []       = []
   wrap _ cs []       = [cs]
   wrap n cs (w : ws) =
      let n' = length w
          n'' = n + 1 + n'
      in if n == 0 then
            wrap n' w ws
         else if n'' <= width then
            wrap n'' (cs ++ ' ' : w) ws
         else
            cs : wrap n' w ws
\end{code}

\submodule{Justification} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{lJustify}~$w~\mathit{cs}$ pads $\mathit{cs}$
with extra spaces on the right to make the overall
width not less than $w$.
\highlighttt{rJustify}~$w~\mathit{cs}$ pads $\mathit{cs}$ with
extra spaces on the left to make the overall width
not less than $w$.

\begin{code}
lJustify, rJustify :: Int -> String -> String
lJustify = lJustify' ' '
rJustify = rJustify' ' '
\end{code}
	   
\noindent \highlighttt{lJustify'}~$p~w~\mathit{cs}$ pads
$\mathit{cs}$ with extra pad characters $p$ on the right to
make the overall width not less than $w$.
\highlighttt{rJustify'}~$p~w~\mathit{cs}$ pads $\mathit{cs}$ with
extra pad characters $p$ on the left to make the
overall width not less than $w$.

\begin{code}
lJustify', rJustify' :: Char -> Int -> String -> String
lJustify' p w cs = 
   let lcs = length cs
   in if lcs >= w
      then cs
      else cs ++ take (w - lcs) (repeat p)
rJustify' p w cs =
   let lcs = length cs
   in if lcs >= w
      then cs
      else take (w - lcs) (repeat p) ++ cs
\end{code}

\submodule{Tables with justified columns} %%%%%%%%%

\noindent \highlighttt{justifyColumn}~$j~\mathit{col}$ justifies
all of the strings in $\mathit{col}$ using $j$ to justify them all
to the same width, which is the width of the widest
string in $\mathit{col}$.
   
\begin{code}
justifyColumn :: (Int -> String -> String) ->
   [String] -> [String]
justifyColumn j css = case css of
   []  -> []
   css ->  
      let w = maximum $ map length css
      in map (j w) css
\end{code}
    
\noindent \highlighttt{makeTable}~$\mathit{js}~\mathit{cols}$ applies
the justification functions in $\mathit{js}$ to the corresponding
columns in $\mathit{cols}$ and assembles the final table.
Short columns have extra blank rows added at the bottom.

\begin{code}
makeTable :: [Int -> String -> String] -> 
   [[String]] -> String
makeTable js css = case css of
   []  -> "\n"
   css -> 
     let h = maximum $ map length css
         pad col = col ++ replicate (h - length col) ""
     in unlines $ map concat $ transpose $ 
        zipWith justifyColumn js $ map pad css
\end{code}

\noindent \highlighttt{spaceColumns}~$\mathit{cs}~\mathit{cols}$ spaces out
columns $\mathit{cols}$ by inserting columns of replicated strings
$\mathit{cs}$.

\begin{code}
spaceColumns :: String -> [[String]] -> [[String]]
spaceColumns cs css = case css of
   []  -> []
   css -> 
     let h = maximum $ map length css
         spaceCol = replicate h cs
     in intersperse spaceCol css
\end{code}

\noindent \highlighttt{makeTableL}~$c~\mathit{cols}$ makes
a table from $\mathit{cols}$ using all left justification,
with $c$ used to pad columns and separate columns.

\begin{code}
makeTableL :: Char -> [[String]] -> String
makeTableL c cols = makeTable (repeat (lJustify' c)) $
   spaceColumns [c] cols
\end{code}

\noindent \highlighttt{makeTableMR}~$\mathit{js}~\mathit{rows}$ makes a
table from elements that are themselves multi-rowed.
$\mathit{js}$ is a list of justifiers for each column (as in 
{\tt makeTable}. $\mathit{rows}$ is a list of rows, where each row
is a list of columns.

\begin{code}
makeTableMR :: [Int -> String -> String] -> 
   [[[String]]] -> String
makeTableMR js =
   let padcs cols = 
          let h = maximum $ map length cols
              pad col = col ++ replicate (h - length col) ""
          in map pad cols
   in makeTable js . map concat . transpose . map padcs
\end{code}

\submodule{Fields} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

These are functions for 
breaking a string into a list of fields and 
converting a list of fields into a string. The fields
are delimited with a nominated special character. To 
permit the special character to appear in a field it 
is preceded by a nominated escape character. To permit
the escape character to appear in a string, it is 
preceded by itself.

\noindent \highlighttt{fields}~$d~e~\mathit{cs}$ breaks
string $\mathit{cs}$ into a list of strings at each delimit
character $d$, removing escape characters $e$ where
appropriate. If the ecsaping is not required use
{\tt ABR.List.chop} instead.

\begin{code}
fields :: Char -> Char -> String -> [String]
fields d e cs = case cs of
      [] -> []
      _  -> f [] [] cs
   where
   f css cs' cs = case cs of
      []      ->
         reverse (reverse cs' : css)
      [c]     ->
         if c == d then
            reverse ([] : reverse cs' : css)
         else 
            reverse (reverse (c:cs') : css)
      c:c':cs ->
         if c == e && c' == d then
            f css (d:cs') cs
         else if c == e && c' == e then
            f css (e:cs') cs
         else if c == d then
            f (reverse cs' : css) [] (c':cs)
         else 
            f css (c:cs') (c':cs)
\end{code}

\noindent \highlighttt{unfields}~$d~e~\mathit{css}$
converts $\mathit{css}$ into one string, with each field
separated by the delimit character $d$, adding
escape characters $e$ as needed. If the ecsaping is not
required use {\tt Data.List.intersperse} instead.

\begin{code}
unfields :: Char -> Char -> [String] -> String
unfields d e css = case css of
      []         -> []
      [cs]       -> escape cs
      cs:cs':css -> escape cs ++ [d] 
                    ++ unfields d e (cs':css)
   where
   escape :: String -> String
   escape []      = []
   escape (c:cs) 
      | c == d    = e : d : escape cs
      | c == e    = e : e : escape cs
      | otherwise = c : escape cs
\end{code}


\submodule{Whitespace} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{trim}~$\mathit{cs}$ strips any
whitespace from both ends of $\mathit{cs}$.

\begin{code}
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse 
       . dropWhile isSpace
\end{code}

\noindent \highlighttt{fixNewlines}~$\mathit{cs}$ rectifies
the ends of lines in $\mathit{cs}$. It does not ensure that
the last character is a newline.

\begin{code}
fixNewlines :: String -> String
fixNewlines cs = case cs of
   '\r' : '\n' : cs' -> '\n' : fixNewlines cs'
   '\r' :  cs'       -> '\n' : fixNewlines cs'
   c : cs'           -> c : fixNewlines cs'
   []                -> []
\end{code}

\noindent \highlighttt{fixNewlines'}~$\mathit{cs}$ rectifies
the ends of lines in $\mathit{cs}$. This version ensures
that the last line is complete, {\it i.e.} that
unless $\mathit{cs}$ is empty, the last character returned
will be a newline. 

\begin{code}
fixNewlines' :: String -> String
fixNewlines' cs = case cs of
   '\n' : []         -> '\n' : []
   '\r' : []         -> '\n' : []
   '\r' : '\n' : []  -> '\n' : []
   c : []            -> c : '\n' : []
   '\r' : '\n' : cs' -> '\n' : fixNewlines cs'
   '\r' :  cs'       -> '\n' : fixNewlines cs'
   c : cs'           -> c : fixNewlines cs'
   []                -> []
\end{code}

\noindent \highlighttt{spaces'}~$n$ returns $n$ spaces. 

\begin{code}
spaces :: Int -> String
spaces n = replicate n ' '
\end{code}

\submodule{Pattern matching and substitution} %%%%%%%%%%%%%

\noindent
\highlighttt{findClosest}~$\mathit{pattern}~\mathit{candiates}$
returns the position in $\mathit{candidates}$ of the string
which, ignoring case is closest to $\mathit{pattern}$ or
$-1$ if $\mathit{candidates}$ is empty.

\begin{code}
findClosest :: String -> [String] -> Int
findClosest pattern candidates
   = fc (-1) (-1) (-1) $ map (map toUpper) candidates  
     where
     p = map toUpper pattern
     fc _ pos _ []
        = pos
     fc run pos n (c:cs)
        | run >= run' = fc run pos (n+1) cs
        | otherwise   = fc run' (n+1) (n+1) cs
        where
        run' = 
           length $ takeWhile (\(a,b) -> a == b) $ zip c p
\end{code}

\noindent \highlighttt{substs}~$\mathit{prs}~\mathit{cs}$ performs
substitutions on $\mathit{cs}$. $\mathit{prs}$ is a list of pairs
$(p,r)$, where $p$ is a case sensitive pattern to
be replaced by $r$ wherever it occurs. 

\begin{code}
substs :: [(String, String)] -> String -> String
substs prs = s prs
   where
   s _           []       = []
   s []          (c:cs)   = c : s prs cs
   s ((p,r):prs') cs      
      | p `isPrefixOf` cs =
         r ++ s prs (drop (length p) cs)
      | otherwise         = s prs' cs   
\end{code}

\noindent \highlighttt{subst}~$p~r~\mathit{cs}$ performs
substitutions on $\mathit{cs}$. $p$ is a case sensitive
pattern to be replaced by $r$ wherever it occurs. 

\begin{code}
subst :: String -> String -> String -> String
subst p r = substs [(p,r)]
\end{code}

\noindent \highlighttt{subHashNums}~$\mathit{rs}~\mathit{cs}$
performs substitutions on $\mathit{cs}$. $\mathit{rs}$ is a list of
replacements. $r$\verb+!!+$0$ will replace the
pattern \verb+#0#+, $r$\verb+!!+$1$ will replace
the pattern \verb+#1#+, ...

\begin{code}
subHashNums :: [String] -> String -> String
subHashNums rs = substs (zipWith make rs [0..])
   where
   make r n = ('#' : show n ++ "#", r) 
\end{code}

\submodule{Numbers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{isCardinal},  \highlighttt{isFixed},
\highlighttt{isFloat}, \highlighttt{isSignedCardinal},
\highlighttt{isSignedFixed} and \highlighttt{isSignedFloat} 
are all predicated that test whether a string could be parsed
as a number.

\begin{code}
isCardinal, isFixed, isFloat, isSignedCardinal, 
   isSignedFixed, isSignedFloat :: String -> Bool
isCardinal cs = case checkLex cardinalL cs of
   CheckPass _ -> True
   _           -> False
isFixed cs = case checkLex fixedL cs of
   CheckPass _ -> True
   _           -> False
isFloat cs = case checkLex floatL cs of
   CheckPass _ -> True
   _           -> False
isSignedCardinal cs = case checkLex signedCardinalL cs of
   CheckPass _ -> True
   _           -> False
isSignedFixed cs = case checkLex signedFixedL cs of
   CheckPass _ -> True
   _           -> False
isSignedFloat cs = case checkLex signedFloatL cs of
   CheckPass _ -> True
   _           -> False
\end{code}


\submodule{Names} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{nameLT}~$n_{1}$~$n_{2}$
returns {\tt True} if name $ n_{1}< n_{2}$. Use
this to sort names with {\tt msort} when names are
in {\it family-name comma other-names} format.

\begin{code}
nameLT :: String -> String -> Bool
nameLT xs ys 
   = f xs < f ys
     where
     f = map toUpper . filter (`notElem` " -'")
\end{code}

\submodule{Path catenation operators} %%%%%%%%%%%%%%%%%%%%%

\highlighttt{++/++} joins two paths with a single
\verb"/". \highlighttt{++.++} joins two paths with
a single \verb"." The utility of these operators is
that any extra \verb"/"s or \verb"."s at the join
are removed.

\begin{code}
infixl 6 ++/++, ++.++
\end{code}

\begin{code}
(++/++), (++.++) :: String -> String -> String
(++/++) = catenateWith '/'
(++.++) = catenateWith '.'
\end{code}

\noindent More such operators can be constructed
with \\
\highlighttt{catenateWith}~$c~\mathit{cs}~\mathit{cs}'$,
which catenates $\mathit{cs}$ and $\mathit{cs}'$ with exactly one $c$
at the join.

\begin{code}
catenateWith :: Char -> String -> String -> String
catenateWith c cs cs' = 
      reverse (dropWhile (== c) (reverse cs))
   ++ [c]
   ++ dropWhile (== c) cs'
\end{code}

\submodule{Simple String Delimitation} %%%%%%%%%%%%%%%%%%%%%


\noindent \highlighttt{unString}~$s$ rectifies string $s$,
by removing the double quotes from each end (if present)
and replacing pairs of double quotes with just one. If
there are no double quotes in $s$, it is returned
unchanged.

\begin{code}
unString :: String -> String
unString s = case s of
   []             -> []
   ['"']          -> []
   ['"','"']      -> []
   '"' : '"' : cs -> '"' : unString cs
   '"' : cs       -> unString cs
   c : cs         -> c : unString cs
\end{code}

\noindent \highlighttt{enString}~$s$ encodes a string
with enclosing quotes and doubles any enclosed quotes.

\begin{code}
enString :: String -> String
enString s = '"' : dbl s ++ "\""
   where 
   dbl cs = case cs of 
      []     -> []
      '"':cs -> "\"\"" ++ dbl cs
      c  :cs -> c : dbl cs
\end{code}


