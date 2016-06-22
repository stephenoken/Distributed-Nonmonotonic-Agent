\module{Histories} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The module {\tt History} implements a data structure
for storage and recall of prior proof results.

\begin{code}
module History(
      History, emptyHistory, addProof, getResult,
      retractProof
   ) where
\end{code}

\begin{code}
import ABR.Data.BSTree
import DebugUtils.Trace
\end{code}

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A history is a record of the result of each proof
attempted.

\begin{code}
type History proof result = BSTree proof result
\end{code}


\submodule{Methods} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This is an empty History.

\begin{code}
emptyHistory :: Ord proof => History proof result
emptyHistory = emptyBST
\end{code}


This adds a proof and status to the History.

\begin{code}
addProof :: (Show proof, Show result, Ord proof) => History proof result -> proof
            -> result -> History proof result
addProof h p s = updateBST (\x _ -> x) p s h
\end{code}


This retrieves a {\tt ProofResult}.

\begin{code}
getResult :: Ord proof => History proof result -> proof
             -> Maybe result
getResult h p = trace ("In History.lhs getResult line 52 ")
  lookupBST p h
\end{code}


\noindent {\tt retractProof}~$h$~$p$ retracts the
result stored in $h$ for $p$ if it exists.

\begin{code}
retractProof :: Ord proof => History proof result
   -> proof -> History proof result
retractProof h p = deleteBST p h
\end{code}
