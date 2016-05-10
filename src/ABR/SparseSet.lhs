% SparseSet.lhs
% This file was produced from SparseSet.lit

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

\module{Sparse Set Type} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.SparseSet} implements a set type where the
elements are orderable, but too selected from too
large a domain to make an array implementation
practical.

\begin{code}
module ABR.SparseSet (
      SparseSet, emptySS, nullSS, insertSS, mkSS,
      deleteSS, elemSS, notElemSS, flattenSS, list2SS,
      countSS, isSubSet, unionSS, sectSS, diffSS
   ) where
\end{code}

\begin{code}
import ABR.Data.BSTree
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Requires review.


\submodule{Data type}

A \highlighttt{SparseSet} is a implemented with a
height-balanced tree.

\begin{code}
type SparseSet a =
   BSTree a ()
\end{code}

\submodule{Operations}

\highlighttt{emptySS} is $\{\}$.

\begin{code}
emptySS :: Ord k => SparseSet k
emptySS = emptyBST
\end{code}

\noindent \highlighttt{nullSS}~$s$ returns {\tt
True} iff $s = \{\}$.

\begin{code}
nullSS :: Ord k => SparseSet k -> Bool
nullSS = nullBST
\end{code}

\noindent \highlighttt{insertSS}~$e~s$ returns
$\{e\} \cup s$.

\begin{code}
insertSS :: Ord k => k -> SparseSet k -> SparseSet k
insertSS k = updateBST (\x _ -> x) k ()
\end{code}

\noindent \highlighttt{mkSS}~$e$ returns
$\{e\}$.

\begin{code}
mkSS :: Ord k => k -> SparseSet k
mkSS e = insertSS e emptySS
\end{code}

\noindent \highlighttt{deleteSS}~$s~e$ returns $s
- \{e\}$.

\begin{code}
deleteSS :: Ord k =>  SparseSet k -> k -> SparseSet k
deleteSS = flip deleteBST
\end{code}

\noindent \highlighttt{elemSS}~$s$ returns {\tt
True} iff $e \in s$.

\begin{code}
elemSS :: Ord k => k -> SparseSet k -> Bool
elemSS = memberBST
\end{code}

\noindent \highlighttt{notElemSS}~$s$ returns {\tt
True} iff $e \not\in s$.

\begin{code}
notElemSS :: Ord k => k -> SparseSet k -> Bool
notElemSS e s = not $ memberBST e s
\end{code}

\noindent \highlighttt{isSubSet}~$a~b$ returns
{\tt True} iff $a \subseteq b$.

\begin{code}
isSubSet :: Ord k => SparseSet k -> SparseSet k -> Bool
isSubSet a b = and $ map (`elemSS` b) $ flattenSS a
\end{code}

\noindent \highlighttt{flattenSS}~$s$ returns the
list of elements of $s$ in ascending order.

\begin{code}
flattenSS :: Ord k => SparseSet k -> [k]
flattenSS = domBST
\end{code}

\noindent \highlighttt{list2SS}~$\mathit{xs}$ returns the
set of elements in $\mathit{xs}$.

\begin{code}
list2SS :: Ord k => [k] -> SparseSet k
list2SS xs = list2BST xs ()
\end{code}

\noindent \highlighttt{countSS}~$s$ returns $|s|$.

\begin{code}
countSS :: Ord k => SparseSet k -> Int
countSS = countBST
\end{code}

\noindent \highlighttt{unionSS}~$a~b$ returns $a
\cup b$. This is faster if $|a| < |b|$.

\begin{code}
unionSS :: Ord k => SparseSet k -> SparseSet k
                    -> SparseSet k
unionSS a b = foldr insertSS b $ flattenSS a
\end{code}

\noindent \highlighttt{sectSS}~$a~b$ returns $a
\cap b$. This is faster if $|a| < |b|$.

\begin{code}
sectSS :: Ord k => SparseSet k -> SparseSet k
                   -> SparseSet k
sectSS a b = list2SS $ filter (`elemSS` b) $ flattenSS a
\end{code}

\noindent \highlighttt{diffSS}~$a~b$ returns $a
- b$.

\begin{code}
diffSS :: Ord k => SparseSet k -> SparseSet k
                   -> SparseSet k
diffSS a b = foldl deleteSS a $ flattenSS b
\end{code}
