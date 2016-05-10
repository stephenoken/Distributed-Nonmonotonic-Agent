% Format.lhs
% This file was produced from HaskellLexer.lit

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

\module{Haskell Lexer} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The module \highlighttt{ABR.HaskellLexer} provides facilities to
partially parse Haskell sources. 

\begin{code}
module ABR.HaskellLexer (
      deliterate, programL, offside, unlex, promoteMethods,
      discardInners, moduleName, declared, declarations
   ) where
\end{code}

\begin{code}
import Data.Char
\end{code}

\begin{code}
import ABR.Parser.Pos; import ABR.Parser
import ABR.Parser.Lexers hiding (floatL, stringL)
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Requires review.
   

\submodule{Handling literate scripts} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{deliterate}~$\mathit{cps}$ removes
all informal text from $\mathit{cps}$, a literate Haskell
source as a list of character-position pairs as
produced by {\tt Parser.preLex}. A similar list of 
character-position pairs is returned. This does not
remove \verb"--" or \verb"{-" \verb"-}" comments
from within the formal text. Those comments are
handled by the lexer.

\begin{code}
deliterate :: [(Char, Pos)] -> [(Char, Pos)]
\end{code}

\noindent Figure~\ref{delit-fsm} shows a finite
state machine that has been extended to include
\LaTeX\ literate scripts. In that figure $\bullet >$
 is a $>$ at the start of a line, \verb"*"
represents any character other than those that are
shown to lead to other states. All states are
accepting states. It is implemented as follows:

\begin{figure}[t!]
   \centering
   \epsfig{figure=figures/HLexer/delit-FSM}
   \caption{\CrampedFontSize\label{delit-fsm}
            The finite state machine for deliterating
	    Haskell sources.}
\end{figure}

\begin{code}
deliterate = i
   where
\end{code}

\begin{code}
   i cps = case cps of
      []                -> []
      (('>',(_,0)):cps) -> f' cps
      (('\\',p):cps)    -> i1 cps 
      (_:cps)           -> i cps
\end{code}

\begin{code}
   f' cps = case cps of
      []             -> []
      (('\n',p):cps) -> ('\n',p) : i cps
      (cp:cps)       -> cp : f' cps
\end{code}

\begin{code}
   ix c a cps = case cps of
      []           -> []
      ((c',p):cps) -> if c == c' then
            a cps
         else
	    i cps
\end{code}

\begin{code}
   i1  = ix 'b' i2
   i2  = ix 'e' i3
   i3  = ix 'g' i4
   i4  = ix 'i' i5
   i5  = ix 'n' i6
   i6  = ix '{' i7
   i7  = ix 'c' i8
   i8  = ix 'o' i9
   i9  = ix 'd' i10
   i10 = ix 'e' i11
   i11 = ix '}' f
\end{code}
         
\begin{code}
   f cps = case cps of
      []             -> []
      (('\\',p):cps) -> f1 [('\\',p)] cps 
      (cp:cps)       -> cp : f cps
\end{code}

\begin{code}
   fx c a rs cps = case cps of
      []           -> reverse rs
      ((c',p):cps) -> if c == c' then
            a ((c',p):rs) cps
         else
	    reverse rs ++ [(c',p)] ++ f cps
\end{code}

\begin{code}
   f1 = fx 'e' f2
   f2 = fx 'n' f3
   f3 = fx 'd' f4
   f4 = fx '{' f5
   f5 = fx 'c' f6
   f6 = fx 'o' f7
   f7 = fx 'd' f8
   f8 = fx 'e' f9
   f9 = fx '}' (\_ cps -> i cps)
\end{code}
   

\submodule{Lexing scripts} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This section implements a lexer for Haskell. It is
essentially complete for ASCII sources, but not for
unicode sources. 

\noindent \highlighttt{programL} performs the
lexical analysis of any Haskell source. Apply {\tt
deliterate} to literate sources \emph{before}
lexing.

\InputEBNF{HaskellLexer}{program}

\begin{code}
programL :: Lexer
programL = listL [hwhitespaceL, lexemeL]
\end{code}

\InputEBNF{HaskellLexer}{lexeme}

\begin{code}
lexemeL :: Lexer
lexemeL = 
       varidL <|> conidL <|> varsymL <|> consymL 
   <|> hliteralL <|> specialL <|> reservedopL
   <|> reservedidL
\end{code}

\InputEBNF{HaskellLexer}{literal}

\begin{code}
hliteralL :: Lexer
hliteralL = integerL <|> floatL <|> charL <|> stringL
\end{code}

\InputEBNF{HaskellLexer}{special}

\begin{code}
specialL :: Lexer
specialL = (
          literalL '('  <|> literalL ')'  <|> literalL ','
      <|> literalL ';'  <|> literalL '['  <|> literalL ']'
      <|> literalL '_'  <|> literalL '`'  <|> literalL '{'
      <|> literalL '}'
   ) %> "special"
\end{code}

\InputEBNF{HaskellLexer}{whitespace}

\begin{code}
hwhitespaceL :: Lexer
hwhitespaceL = (some whitestuffL) *%> " "
\end{code}

\InputEBNF{HaskellLexer}{whitestuff}

\begin{code}
whitestuffL :: Lexer
whitestuffL = 
   (whitecharL <|> commentL <|> ncommentL) %> " "
\end{code}

\InputEBNF{HaskellLexer}{whitechar}

\begin{code}
whitecharL :: Lexer
whitecharL = 
       newlineL <|> vertabL <|> formfeedL <|> spaceL
   <|> tabL <|> nonbrkspcL
\end{code}

These are already defined in Parser.lhs.

\InputEBNF{Parser}{newline}
\InputEBNF{Parser}{space}
\InputEBNF{Parser}{tab}
\InputEBNF{Parser}{vertab}
\InputEBNF{Parser}{formfeed}

\InputEBNF{HaskellLexer}{nonbrkspc}

\begin{code}
nonbrkspcL :: Lexer
nonbrkspcL = failA "non-breaking spaces not implemented"
\end{code}

\InputEBNF{HaskellLexer}{comment}

\begin{code}
commentL :: Lexer
commentL = tokenL "--" <* many any1L <* newlineL
\end{code}

\InputEBNF{HaskellLexer}{ncomment}

\begin{code}
ncommentL :: Lexer
ncommentL =
   tokenL "{-" <* aNYseqL 
   <* soft (many (ncommentL <* aNYseqL)) <* tokenL "{-"
\end{code}

\InputEBNF{HaskellLexer}{ANYseq}

\begin{code}
aNYseqL :: Lexer
aNYseqL = 
   soft (manyUntil any2L (tokenL "{-" <|> tokenL "-}"))
\end{code}

\InputEBNF{HaskellLexer}{any2}

\begin{code}
any2L :: Lexer
any2L = any1L <|> newlineL <|> vertabL <|> formfeedL
\end{code}

\InputEBNF{HaskellLexer}{any1}

\begin{code}
any1L :: Lexer
any1L = graphicL <|> spaceL <|> tabL <|> nonbrkspcL
\end{code}

\InputEBNF{HaskellLexer}{graphic}

\begin{code}
graphicL :: Lexer
graphicL =
      largeL <|> smallL <|> digitL <|> symbolL <|> specialL 
   <|> literalL ':' <|> literalL '"' <|> literalL '\''
\end{code}

\InputEBNF{HaskellLexer}{small}

\begin{code}
smallL :: Lexer
smallL = aSCsmallL <|> iSOsmallL
\end{code}

\InputEBNF{HaskellLexer}{ASCsmall}

\begin{code}
aSCsmallL :: Lexer
aSCsmallL = satisfyL isLower "ASCsmall"
\end{code}

\InputEBNF{HaskellLexer}{ISOsmall}

\begin{code}
iSOsmallL :: Lexer
iSOsmallL = failA "ISOsmall not implemented"
\end{code}

\InputEBNF{HaskellLexer}{large}

\begin{code}
largeL :: Lexer
largeL = aSClargeL <|> iSOlargeL
\end{code}

\InputEBNF{HaskellLexer}{ASClarge}

\begin{code}
aSClargeL :: Lexer
aSClargeL = satisfyL isUpper "ASClarge"
\end{code}

\InputEBNF{HaskellLexer}{ISOlarge}

\begin{code}
iSOlargeL :: Lexer
iSOlargeL = failA "ISOlarge not implemented"
\end{code}

\InputEBNF{HaskellLexer}{symbol}

\begin{code}
symbolL :: Lexer
symbolL = aSCsymbolL <|> iSOsymbolL
\end{code}

\InputEBNF{HaskellLexer}{ASCsymbol}

\begin{code}
aSCsymbolL :: Lexer
aSCsymbolL =
       literalL '!'  <|> literalL '#'  <|> literalL '$'
   <|> literalL '%'  <|> literalL '&'  <|> literalL '*'
   <|> literalL '+'  <|> literalL '.'  <|> literalL '/'
   <|> literalL '<'  <|> literalL '='  <|> literalL '>'
   <|> literalL '?'  <|> literalL '@'  <|> literalL '\\'
   <|> literalL '^'  <|> literalL '|'  <|> literalL '-'
   <|> literalL '~'
\end{code}

\InputEBNF{HaskellLexer}{ISOsymbol}

\begin{code}
iSOsymbolL :: Lexer
iSOsymbolL = failA "ISOsymbol not implemented"
\end{code}

\InputEBNF{HaskellLexer}{digit}

\begin{code}
digitL :: Lexer
digitL = satisfyL isDigit "digit"
\end{code}

\InputEBNF{HaskellLexer}{octit}

\begin{code}
octitL :: Lexer
octitL = satisfyL (\c -> c >= '0' && c <= '7') "octit"
\end{code}

\InputEBNF{HaskellLexer}{hexit}

\begin{code}
hexitL :: Lexer
hexitL = satisfyL isHexDigit "hexit"
\end{code}

\InputEBNF{HaskellLexer}{varid}

\begin{code}
varidL' :: Lexer
varidL' = (
      smallL <**>
      ((many (smallL <|> largeL <|> digitL
              <|> literalL '\'' <|> literalL '_')) *%> "")
   ) %> "varid"
\end{code}

\begin{code}
varidL :: Lexer
varidL = 
   dataSatisfies varidL' 
   (\(((_,ident),_):_) -> not (ident `elem` reservedids))
\end{code}

\InputEBNF{HaskellLexer}{conid}

\begin{code}
conidL :: Lexer
conidL = ( 
      largeL <**>
      ((many (smallL <|> largeL <|> digitL <|> literalL '\''
              <|> literalL '_')) *%> "")
   ) %> "conid"
\end{code}

\InputEBNF{HaskellLexer}{reservedid}

\begin{code}
reservedids :: [String]
reservedids = [
      "case",     "class",    "data",  "default",
      "deriving", "do",       "else",  "if",
      "import",   "in",       "infix", "infixl",
      "infixr",   "instance", "let",   "module",
      "newtype",  "of",       "then",  "type",
      "where"
   ]
\end{code}

\begin{code}
reservedidL :: Lexer
reservedidL = (
      dataSatisfies varidL' 
      (\(((_,id),_):_) -> id `elem` reservedids)
   ) %> "reservedid"
\end{code}

\InputEBNF{HaskellLexer}{specialid}

\begin{code}
specialids :: [String]
specialids = ["as", "qualified", "hiding"]
\end{code}

\InputEBNF{HaskellLexer}{varsym}

\begin{code}
varsymL' :: Lexer
varsymL' = ( symbolL <**>
             ((many (symbolL <|> literalL ':')) *%> "")
           ) %> "varsym"
\end{code}

\begin{code}
varsymL :: Lexer
varsymL = 
   dataSatisfies varsymL' 
   (\(((_,sym),_):_) -> not (sym `elem` reservedops))
\end{code}

\InputEBNF{HaskellLexer}{consym}

\begin{code}
consymL' :: Lexer
consymL' = ( literalL ':' <**>
             ((many (symbolL <|> literalL ':')) *%> "")
           ) %> "varsym"
\end{code}

\begin{code}
consymL :: Lexer
consymL =
   dataSatisfies consymL'
   (\(((_,sym),_):_) -> not (sym `elem` reservedops))
\end{code}

\InputEBNF{HaskellLexer}{reservedop}

\begin{code}
reservedops :: [String]
reservedops = ["..", "::", "=", "\\", "|", "<-", "->",
               "@",  "~",  "=>"]
\end{code}

\begin{code}
reservedopL :: Lexer
reservedopL = (
          tokenL ".."  <|> tokenL "::" <|> tokenL "=>"
      <|> tokenL "="  <|> tokenL "\\" <|> tokenL "|"
      <|> tokenL "<-" <|> tokenL "->" <|> tokenL "@"
      <|> tokenL "~" 
   ) %> "reservedop"
\end{code}

\InputEBNF{HaskellLexer}{specialop}

\begin{code}
specialopL :: [String]
specialopL = ["-", "!"]
\end{code}

\InputEBNF{HaskellLexer}{decimal}

\begin{code}
decimalL :: Lexer
decimalL = (some digitL) *%> "decimal"
\end{code}

\InputEBNF{HaskellLexer}{octal}

\begin{code}
octalL :: Lexer
octalL = (some octitL) *%> "octal"
\end{code}

\InputEBNF{HaskellLexer}{hexadecimal}

\begin{code}
hexadecimalL :: Lexer
hexadecimalL = (some hexitL) *%> "hexadecimal"
\end{code}

\InputEBNF{HaskellLexer}{integer}

\begin{code}
integerL :: Lexer
integerL = (
          decimalL
      <|> (literalL '0' <**> literalL 'o' <**> octalL)
      <|> (literalL '0' <**> literalL 'O' <**> octalL)
      <|> (literalL '0' <**> literalL 'x' <**>
           hexadecimalL)
      <|> (literalL '0' <**> literalL 'X' <**> 
           hexadecimalL)
   ) %> "integer"
\end{code}

\InputEBNF{HaskellLexer}{float}

\begin{code}
floatL :: Lexer
floatL = (
      decimalL <**> literalL '.' <**> decimalL <**>
      ((optional ((literalL 'e' <|> literalL 'E')
       <**> ((optional (literalL '-' <|> literalL '+')
            ) *%> ""
                 )
                           <**> decimalL
                          )
                ) *%> ""
               )
   ) %> "float"
\end{code}

\InputEBNF{HaskellLexer}{char}

\begin{code}
charL :: Lexer
charL = (     literalL '\''
         <**> (    alsoNotSat graphicL not1L
               <|> spaceL
               <|> alsoNotSat escapeL not2L
              )
         <**> literalL '\''
        ) %> "char"
        where
        not1L :: Lexer
        not1L = literalL '\'' <|> literalL '\\'
        not2L :: Lexer
        not2L = literalL '\\' <**> literalL '&'
\end{code}

\InputEBNF{HaskellLexer}{string}

\begin{code}
stringL :: Lexer
stringL = (     literalL '"'
           <**> ((many (    alsoNotSat graphicL not1L
                        <|> spaceL
                        <|> escapeL
                        <|> gapL
                       )
                 ) *%> ""
                )
           <**> literalL '"'
          ) %> "string"
          where
          not1L :: Lexer
          not1L = literalL '"' <|> literalL '\\'
\end{code}

\InputEBNF{HaskellLexer}{escape}

\begin{code}
escapeL :: Lexer
escapeL = (     literalL '\\'
           <**> (    charescL
                 <|> asciiL
                 <|> decimalL
                 <|> (literalL 'o' <**> octalL)
                 <|> (literalL 'x' <**> hexadecimalL)
                )
          ) %> "escape"
\end{code}

\InputEBNF{HaskellLexer}{charesc}

\begin{code}
charescL :: Lexer
charescL = (
          literalL 'a'  <|> literalL 'b'  <|> literalL 'f'
      <|> literalL 'n'  <|> literalL 'r'  <|> literalL 't'
      <|> literalL 'v'  <|> literalL '\\' <|> literalL '"'
      <|> literalL '\'' <|> literalL '&'
   ) %> "charesc"
\end{code}

\InputEBNF{HaskellLexer}{ascii}

\begin{code}
asciiL :: Lexer
asciiL = (
          (literalL '^' <**> cntrlL)
      <|> tokenL "NUL" <|> tokenL "SOH" <|> tokenL "STX"
      <|> tokenL "ETX" <|> tokenL "EOT" <|> tokenL "ENQ"
      <|> tokenL "ACK" <|> tokenL "BEL" <|> tokenL "BST"
      <|> tokenL "LF"  <|> tokenL "VT"  <|> tokenL "FF" 
      <|> tokenL "CR"  <|> tokenL "SO"  <|> tokenL "SI"
      <|> tokenL "DLE" <|> tokenL "DC1" <|> tokenL "DC2"
      <|> tokenL "DC3" <|> tokenL "DC4" <|> tokenL "NAK"
      <|> tokenL "SYN" <|> tokenL "ETB" <|> tokenL "CAN"
      <|> tokenL "EM"  <|> tokenL "SUB" <|> tokenL "FS"
      <|> tokenL "GS"  <|> tokenL "RS"  <|> tokenL "US"
      <|> tokenL "SP"  <|> tokenL "DEL"
   ) %> "ascii"
\end{code}

\InputEBNF{HaskellLexer}{cntrl}

\begin{code}
cntrlL :: Lexer
cntrlL = (
          aSClargeL     <|> literalL '@' <|> literalL '['
      <|> literalL '\\' <|> literalL ']' <|> literalL '^'
      <|> literalL '_'
   ) %> "cntrl"
\end{code}

\InputEBNF{HaskellLexer}{gap}

\begin{code}
gapL :: Lexer
gapL = (     literalL '\\'
        <**> ((some whitecharL) *%> "")
        <**> literalL '\\'
       ) %> "gap"
\end{code}


\submodule{Handling the offside rule} %%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{offside}~$\mathit{tlps}$ applies the off-side
layout rule, inserting braces and semicolons.
$\mathit{tlps}$ is a list of tag-lexeme-position tuples
produced by the lexer ({\tt programL}) and after
all whitespace has been removed with {\tt
dropwhite}. Note that scripts either start with an
explicit or implicit module header. Either case is
properly handled, as is the case of unexpectedly
short scripts.

\begin{code}
offside :: TLPs -> TLPs
offside []
   = []
offside [x]
   = [x]
offside ((("reservedid", "module"),pos):tlps)
   = os [] ((("reservedid", "module"),pos):tlps)
offside (((tag, lexeme),(line,column)):tlps)
   =   (("special","{"),(line,column)) 
     : ((tag, lexeme),(line,column))
     : os [column] tlps
\end{code}

This function does the real work. The list of {\tt
Int}s is the stack of pending column numbers
defining on/off-side.

\begin{code}
os :: [Int] -> TLPs -> TLPs
os [] []
   = []
os (col:cols) []
   = (("special","}"),(-1,-1)) : os cols []
os [] [((t,le),(li,c))]
   = [((t, le),(li,c))]
os [] (((t, le),(li,c)):((t', le'),(li',c')):tlps)
   | oskeyword t le
      =   ((t, le),(li,c))
        : (("special","{"),(li',c'))
        : os [c'] (((t', le'),(li',c'+1)):tlps)
   | otherwise
      =   ((t, le),(li,c)) 
        : os [] (((t', le'),(li',c')):tlps)
os (col:cols) [((t, le),(li,c))]
   | c < col
      =   (("special","}"),(li,c))
        : os cols [((t, le),(li,c))]
   | c == col
      =   (("special",";"),(li,c))
        : os (col:cols) [((t, le),(li,c+1))]
   | otherwise
      = ((t, le),(li,c)) : os (col:cols) []
os (col:cols) (((t, le),(li,c)):((t', le'),(li',c')):tlps)
   | c < col
      =   (("special","}"),(li,c))
        : os cols (((t, le),(li,c)):((t', le'),(li',c'))
	           : tlps)
   | c == col
      =   (("special",";"),(li,c))
        : os (col:cols)
	     (((t, le),(li,c+1)):((t', le'),(li',c')):tlps)
   | oskeyword t le
      =   ((t, le),(li,c))
        : (("special","{"),(li',c'))
        : os (c':col:cols) (((t', le'),(li',c'+1)):tlps)
   | otherwise
      =   ((t, le),(li,c))
        : os (col:cols) (((t', le'),(li',c')):tlps)
\end{code}

\begin{code}
oskeyword :: String -> String -> Bool
oskeyword tag lexeme
   = tag == "reservedid" && (lexeme `elem` oskeywords)
\end{code}

\begin{code}
oskeywords :: [String]
oskeywords = ["where", "let", "do", "of"]
\end{code}


\submodule{Diagnostics} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{unlex}~$\mathit{tlps}$ undoes all of the above
good work by unrolling all of the lexing of $\mathit{tlps}$.
It should be very useful to check for instance that
the offside rule has been applied properly.

\begin{code}
unlex :: TLPs -> String
unlex
   = ul 0
     where
     ul :: Col -> TLPs -> String
     ul _ []
        = "\n"
     ul indent (((t,le),(li,c)):tlps)
        | t == "special" && le == "{"
           = "{\n" ++ take (indent+3) (repeat ' ') 
	     ++ ul (indent+3) tlps
        | t == "special" && le == ";"
           = "\n" ++  take (indent) (repeat ' ') ++ "; " 
	     ++ ul indent tlps
        | t == "special" && le == "}"
           = "\n" ++ take (indent-3) (repeat ' ') ++ "}\n" 
             ++ ul (indent-3) tlps
        | otherwise
            = le ++ " " ++ ul indent tlps
\end{code}


\submodule{Poor man's parsing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This section contains functions for analysing the
results of the lexing phases above without using a
real (combinator) Parser. This method might turn
out to be good enough to generate the sort of
information required to create the Haskell
dictionary which started me down this path. I have
also used it for logical line counting.

\noindent Call this function BEFORE the next one.
\highlighttt{promoteMethods}~$\mathit{tlps}$ promotes the
definitions within the where clause of class
declatations to the top level.

\begin{code}
promoteMethods :: TLPs -> TLPs
promoteMethods =
   pm 0
   where
   pm :: Int -> TLPs -> TLPs
   -- the Int = 0 for not in class, 1 = in class,
   -- 2 = class with where 3, .. deeper levels
   pm _ [] =
      []
   pm 0 ((("reservedid","class"),p):tlps) =
      (("reservedid","class"),p) : pm 1 tlps
   pm 0 (tlp:tlps) =
      tlp : pm 0 tlps
   pm 1 ((("special",";"),p):tlps) =
      (("special",";"),p) : pm 0 tlps
   pm 1 ((("special","{"),p):tlps) =
      (("special",";"),p) : pm 2 tlps
   pm 2 ((("special","}"),p):tlps) =
      (("special",";"),p) : pm 0 tlps
   pm n ((("special","{"),p):tlps) =
      (("special","{"),p) : pm (n+1) tlps
   pm n ((("special","}"),p):tlps) =
      (("special","}"),p) : pm (n-1) tlps
   pm n (tlp:tlps) =
      tlp : pm n tlps
\end{code}

\noindent This function makes it easier to pick out
top-level declarations.
\highlighttt{discardInners}~$\mathit{tlps}$ filters out all
less-than-top-level declarations, crudely by
eliminating all \{stuff; stuff; ... ; stuff\}
sequences inside the top-level such sequence in
$\mathit{tlps}$.

\begin{code}
discardInners :: TLPs -> TLPs
discardInners
   = di 0
     where
     di :: Int -> TLPs -> TLPs
     di _ []
        = []
     di level ((("special","{"),p):tlps)
        | level <= 0
           = (("special","{"),p) : di 1 tlps
        | otherwise
           = di (level+1) tlps
     di level ((("special","}"),p):tlps)
        | level <= 1
           = (("special","}"),p) : di 0 tlps
        | otherwise
           = di (level-1) tlps
     di level (tlp:tlps)
        | level <= 1
           = tlp : di level tlps
        | otherwise
           = di level tlps
\end{code}

\noindent {\tt discard}~$\mathit{tlps}$ discards all
lexemes from a {\tt TLP} list, $\mathit{tlps}$, up to but
not including the next semi-colon or opening or
closing brace.

\begin{code}
discard :: TLPs -> TLPs
discard []
   = []
discard ((("special","{"),p):tlps)
   = ((("special","{"),p):tlps)
discard ((("special","}"),p):tlps)
   = ((("special","}"),p):tlps)
discard ((("special",";"),p):tlps)
   = ((("special",";"),p):tlps)
discard (tlp:tlps)
   = discard tlps
\end{code}


\noindent \highlighttt{moduleName}~$\mathit{tlps}$ extracts
the name of the module from a {\tt TLP} list,
$\mathit{tlps}$, if there is one, or returns \verb"[]" if
there is none.

\begin{code}
moduleName :: TLPs -> TLPs
moduleName 
   ((("reservedid","module"),_):(("conid",name),pos):_)
   = [(("module",name),pos)]
moduleName _
   = []
\end{code}


\noindent \highlighttt{declarations}~$\mathit{tlps}$ a {\tt
TLP} list, $\mathit{tlps}$, up into its top level
declarations.

\begin{code}
declarations :: TLPs -> [TLPs]
declarations
   = (d [] 0) . (drop 1) . discard
     where
     d :: TLPs -> Int -> TLPs -> [TLPs]
     d [] _ []
        = []
     d [] 0 ((("special","}"),_):_)
        = []
     d [] n ((("special",";"),_):tlps)
        = d [] n tlps
     d [] n (tlp:tlps)
        = d [tlp] n tlps
     d (tlp:tlps) _ []
        = [reverse (tlp:tlps)]
     d (tlp:tlps) n ((("special","}"),pos):tlps')
        | n == 0     
	   = [reverse ((("special","}"),pos):tlp:tlps)]
        | otherwise
	   = d ((("special","}"),pos) : tlp : tlps)
	       (n-1) tlps'
     d (tlp:tlps) n ((("special","{"),pos):tlps')
        = d ((("special","{"),pos) : tlp : tlps)
	    (n+1) tlps'
     d (tlp:tlps) n ((("special",";"),pos):tlps')
        | n == 0    
	   = reverse ((("special",";"),pos):tlp:tlps) 
	    : d [] 0 tlps'
        | otherwise
	   = d ((("special",";"),pos):tlp:tlps) n tlps'
     d (tlp:tlps) n (tlp':tlps')
        = d (tlp':tlp:tlps) n tlps'
\end{code}


\noindent \highlighttt{declared}~$\mathit{tlps}$ takes a
top-level declaration $\mathit{tlps}$ and returns the type
of declaration (as a new set of {\tt Tag}s), the
names of the declared objects ({\tt Lexeme}s) and
the positions of the names of the objects ({\tt
Pos}s). 

\begin{code}
declared :: TLPs -> TLPs
declared ((("reservedid","import"),_):tlps)
   = importD tlps
declared ((("reservedid","infixl"),_):tlps)
   = fixityD tlps
declared ((("reservedid","infixr"),_):tlps)
   = fixityD tlps
declared ((("reservedid","infix"),_):tlps)
   = fixityD tlps
declared ((("reservedid","type"),_):tlps)
   = typeD tlps
declared ((("reservedid","data"),_):tlps)
   = dataD tlps
declared ((("reservedid","newtype"),_):tlps)
   = newtypeD tlps
declared ((("reservedid","class"),_):tlps)
   = classD tlps
declared ((("reservedid","instance"),_):tlps)
   = instanceD tlps
declared tlps 
   = declD tlps
\end{code}

\begin{code}
importD :: TLPs -> TLPs
importD ((("varid","qualified"),pos):tlps) = importD tlps
importD ((("conid",name),pos):_) = [(("import",name),pos)]
importD _ = []
\end{code}

\begin{code}
fixityD :: TLPs -> TLPs
fixityD ((("integer",_),_):tlps) 
   = fixityD tlps
fixityD ((("special",_),_):tlps)
   = fixityD tlps
fixityD ((("consym",name),pos):tlps) 
   = (("fixity",name),pos) : fixityD tlps
fixityD ((("conid",name),pos):tlps) 
   = (("fixity",name),pos) : fixityD tlps
fixityD ((("varid",name),pos):tlps) 
   = (("fixity",name),pos) : fixityD tlps
fixityD ((("varsym",name),pos):tlps) 
   = (("fixity",name),pos) : fixityD tlps
fixityD _ 
   = []
\end{code}

\begin{code}
typeD :: TLPs -> TLPs
typeD ((("conid",name),pos):_) = [(("type",name),pos)]
typeD _ = []
\end{code}

\begin{code}
dataD :: TLPs -> TLPs
dataD tlps
   = case dropContext tlps of
        []          -> dataD' tlps
        (tlp:tlps') -> dataD' (tlp:tlps')
     where
     dataD' ((("conid",name),pos):_) 
        = [(("data",name),pos)]
     dataD' _ 
        = []
\end{code}

\begin{code}
newtypeD :: TLPs -> TLPs
newtypeD tlps
   = case dropContext tlps of
        []          -> newtypeD' tlps
        (tlp:tlps') -> newtypeD' (tlp:tlps')
     where
     newtypeD' ((("conid",name),pos):_)
        = [(("newtype",name),pos)]
     newtypeD' _ 
        = []
\end{code}

\begin{code}
classD :: TLPs -> TLPs
classD tlps
   = case dropContext tlps of
        []          -> classD' tlps
        (tlp:tlps') -> classD' (tlp:tlps')
     where
     classD' ((("conid",name),pos):_) 
        = [(("class",name),pos)]
     classD' _ 
        = []
\end{code}

\begin{code}
instanceD :: TLPs -> TLPs
instanceD tlps
   = case dropContext tlps of
        []          -> instanceD' tlps
        (tlp:tlps') -> instanceD' (tlp:tlps')
     where
     instanceD' 
        ((("conid",name),pos):tlps)
        = [(("instance",name ++ " " 
	   ++ instanceName tlps),pos)]
     instanceD' _
        = error "instance case undefined"
     instanceName :: TLPs -> String
     instanceName (((_,"where"),_):tlps)
        = ""
     instanceName (((_,"("),_):tlps)
        = '(' : instanceName tlps
     instanceName (((_,")"),_):tlps)
        = ')' : instanceName tlps
     instanceName (((_,name),_):tlps)
        = ' ' : name ++ instanceName tlps
\end{code}

\begin{code}
dropContext :: TLPs -> TLPs
dropContext [] = []
dropContext ((("reservedop","=>"),_):tlps) = tlps
dropContext (tlp:tlps) = dropContext tlps
\end{code}

\begin{code}
declD :: TLPs -> TLPs
declD tlps
   = case decider tlps of
        "::" -> typedeclD tlps
        "="  -> valdeclD tlps
        ""   -> []
     where
     decider :: TLPs -> Lexeme
     decider [] = []
     decider ((("reservedop","::"),_):_) = "::"
     decider ((("reservedop","="),_):_) = "="
     decider (_:tlps) = decider tlps
\end{code}

\begin{code}
typedeclD :: TLPs -> TLPs
typedeclD ((("special",_),_):tlps)
   = typedeclD tlps
typedeclD ((("varid",name),pos):tlps)
   = (("variable",name),pos) : typedeclD tlps
typedeclD ((("varsym",name),pos):tlps)
   = (("variable",name),pos) : typedeclD tlps
typedeclD _ = []
\end{code}

\begin{code}
valdeclD :: TLPs -> TLPs
valdeclD (((_,_),pos):tlps) 
   = [(("equation",""),pos)]
\end{code}

