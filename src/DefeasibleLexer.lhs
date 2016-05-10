\module{Lexical Issues} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Various elements of the \deimos\ system parse textual
representations of literals, rules, priorities, theories
and queries. \deimos\ uses the {\tt Parser}
module~\cite{abrhaskelllibs} to
implement functions that perform lexical analysis and
parsers. The {\tt DefeasibleLexer} module implements
the functions for lexical analysis of Defeasible sources.

\begin{code}
module DefeasibleLexer(lexerL) where
\end{code}

\begin{code}
import Data.Char
\end{code}

\begin{code}
import ABR.Parser; import ABR.Parser.Lexers
\end{code}

\submodule{Comments} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Comments in Defeasible sources follow the Prolog
conventions. Comments that start with a percent sign
{\tt \%}) extend to the end of the line.
Comments that start with the sequence {\tt /*} extend
to the the next sequence {\tt */} and may span more
than one line.

Formally, the syntax for each type of comment is:

\input{\texInc comment1Syntax.tex}

\input{\texInc comment2Syntax.tex}

These comment forms are recognized by these lexer
functions.

\begin{code}
comment1L :: Lexer
comment1L
   = tokenL "%"
     <**> (many (satisfyL (/= '\n') "") *%> "")
     <**> (optional (literalL '\n') *%> "")
     %> " "

comment2L :: Lexer
comment2L
   = tokenL "/*" <**> comment2L' %> " "
     where
     comment2L' :: Lexer
     comment2L'
        =     tokenL "*/"
          <|> (satisfyL (\c -> True) "") <**> comment2L'
\end{code}


\submodule{Names} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Literals, rule labels, constants and variables are all
instances of names that occur in Defeasible sources.
Two types are distinguished: those starting with lower
case letters; and those starting with upper-case letters.

Formally, the syntax for each type of name is:

\input{\texInc name1Syntax.tex}

\input{\texInc name2Syntax.tex}

These name forms are recognized by these lexer functions.

\begin{code}
name1L :: Lexer
name1L
   = (satisfyL isLower "lower-case letter" <**>
      ((many (satisfyL isName1Char "letter, digit, _"))
       *%> ""))
     %> "name1"
     where
     isName1Char c = isAlpha c || isDigit c || c == '_'

name2L :: Lexer
name2L
   = (satisfyL isUpper "upper-case letter" <**>
      ((many (satisfyL isName2Char "letter, digit, _"))
       *%> ""))
     %> "name2"
     where
     isName2Char c = isAlpha c || isDigit c || c == '_'
\end{code}


\submodule{Symbols and everything else} %%%%%%%%%%%%%%

This function performs the lexical analysis of a
Defeasible source. It lists all of the symbols that are
special in Defeasible sources.

\begin{code}
lexerL :: Lexer
lexerL
   = dropWhite $ nofail $ total $ listL [
        comment1L,               comment2L,
        tokenL ":=" %> "symbol", tokenL ":^" %> "symbol",
        tokenL ":-" %> "symbol", tokenL "->" %> "symbol",
        tokenL "=>" %> "symbol", tokenL "~>" %> "symbol",
        tokenL "+"  %> "symbol", tokenL "-"  %> "symbol",
        tokenL "~"  %> "symbol", tokenL ">"  %> "symbol",
        tokenL "{"  %> "symbol", tokenL "}"  %> "symbol",
        tokenL "("  %> "symbol", tokenL ")"  %> "symbol",
        tokenL "."  %> "symbol", tokenL ","  %> "symbol",
        tokenL ":"  %> "symbol", name1L,
	name2L,                  whitespaceL
     ]
\end{code}
