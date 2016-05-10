\module{Run-files}

The {\tt DRunFile} module defines a data type for
representing a generator of test cases with
combinations of facts.

\begin{code}
module DRunFile(
      RInput, RIgnore, ROutput, RunFile, Run, runFileP,
      generateRuns
   ) where
\end{code}

\begin{code}
import ABR.Parser; import ABR.List
\end{code}

\begin{code}
import Literal; import DefeasibleLexer
import DInference
\end{code}

\submodule{Data type definitions} %%%%%%%%%%%%%%%%%%%%%%%%%

An {\tt RInput} is one set of mutually exclusive
literals.

\begin{code}
type RInput = [Literal]
\end{code}

\noindent An {\tt RIgnore} is one set of inconsistent
literals.

\begin{code}
type RIgnore = [Literal]
\end{code}

\noindent An {\tt ROutput} is one tagged literal.

\begin{code}
type ROutput = Tagged Literal
\end{code}

\noindent An {\tt RStatement} represents one statement
from a run-file.

\begin{code}
data RStatement =   RInput [Literal]
                  | RIgnore [Literal]
                  | ROutput (Tagged Literal)
\end{code}

\noindent A {\tt Runfile} represents the whole
runfile.

\begin{code}
type RunFile = ([RInput], [RIgnore], [ROutput])
\end{code}

\noindent A {\tt Run} represents one set of generated
facts.

\begin{code}
type Run = [Literal]
\end{code}

\submodule{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The syntax for a runfile is given in
section~\ref{DProverUser}, and is implemented as follows:

\begin{code}
runFileP :: Parser RunFile
runFileP = total $ many (
      (     rInputP  @> RInput
        <|> rIgnoreP @> RIgnore
        <|> rOutputP @> ROutput
      ) ABR.Parser.<* literalP "symbol" "."
   ) @> (foldr (\s (ins,igs,os) -> case s of
            RInput qs  -> (qs : ins, igs,      os     )
            RIgnore qs -> (ins,      qs : igs, os     )
            ROutput tl -> (ins,      igs,      tl : os)
         ) ([],[],[]))
\end{code}

\begin{code}
rInputP :: Parser RInput
rInputP =
      literalP "name1" "input"
    ABR.Parser.*> (     literalP "symbol" "{"
          ABR.Parser.*> pLiteralP
         ABR.Parser.<*> many (
                  literalP "symbol" ","
               ABR.Parser.*> pLiteralP
             )
         ABR.Parser.<*  literalP "symbol" "}"
       ) @> cons
\end{code}

\begin{code}
rIgnoreP :: Parser RIgnore
rIgnoreP =
      literalP "name1" "ignore"
    ABR.Parser.*> (     literalP "symbol" "{"
          ABR.Parser.*> pLiteralP
         ABR.Parser.<*> many (
                  literalP "symbol" ","
               ABR.Parser.*> pLiteralP
             )
         ABR.Parser.<*  literalP "symbol" "}"
       ) @> cons
\end{code}

\begin{code}
rOutputP :: Parser ROutput
rOutputP =
      literalP "name1" "output"
    ABR.Parser.*> (     literalP "symbol" "{"
          ABR.Parser.*> taggedLiteralP
         ABR.Parser.<*  literalP "symbol" "}"
       )
\end{code}


\submodule{Generating runs} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt generateRuns}~{\it run-file} returns the list of
generated set of facts.

\begin{code}
generateRuns :: RunFile -> [Run]
generateRuns (ins,igs,_) =
   filter (\qs -> and [or [q' `notElem` qs
                          | q' <- ig] | ig <- igs]) $
   map concat $
   cartProd $
   map (\qs -> case qs of
      [q] -> [[q], [neg q]]
      _   -> (map (\(b,e,a) -> reverse (map neg b) ++ [e] ++
                  map neg a) . fragments) qs) ins
\end{code}
