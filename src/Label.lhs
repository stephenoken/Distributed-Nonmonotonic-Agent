\module{Labels} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\label{labelModule}

Labels are used to tag rules.

\begin{code}
module Label(
      LabelName, Label(Label), labelP,
      HasLabelNames(getLabelNames)
   ) where
\end{code}

\begin{code}
import ABR.SparseSet; import ABR.Parser; import ABR.DeepSeq
\end{code}

\submodule{Data type definition} %%%%%%%%%%%%%%%%%%%%%%%%%

A {\tt Label} is just a string with a constructor to tag
it as a label.

\begin{code}
type LabelName = String

newtype Label = Label LabelName
                deriving (Eq, Ord)
\end{code}


\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Labels should start with upper case letters.
The syntax for a label is:

\input{\texInc labelSyntax.tex}

\noindent which is implemented:

\begin{code}
labelP :: Parser Label
labelP = tagP "name2" @> (\(_,n,_) -> Label n)
\end{code}


\submodule{Collecting label names} %%%%%%%%%%%%%%%%%%%%%%%

To extract the set of unique name strings from labels
or objects that contains labels, use {\tt getLabelNames},
which accumulates names in a set.

\begin{code}
class HasLabelNames a where

   getLabelNames :: a -> SparseSet LabelName
                    -> SparseSet LabelName

instance HasLabelNames Label where

   getLabelNames (Label n) = insertSS n
\end{code}


\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Show Label where

   showsPrec p (Label n) = showString n
\end{code}

\subsubmodule{Forced evaluation}

\begin{code}
instance DeepSeq Label where
\end{code}

\begin{code}
   deepSeq (Label n) x = deepSeq n x
\end{code}
