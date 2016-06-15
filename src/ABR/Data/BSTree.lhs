% BSTree.lhs
% This file was produced from BSTree.lit

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

\module{Data.BSTree: Balanced Binary Search Tree} %%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Data.BSTree} implements a depth/height balanced
(AVL) binary search tree abstract data type.

\begin{code}
module ABR.Data.BSTree (
      BSTree(..), emptyBST, nullBST, depthBST, updateBST,
      deleteBST, lookupBST, memberBST, lookupGuard,
      flattenBST, domBST, ranBST, countBST, leftBST,
      rightBST, mapBST, pairs2BST, list2BST
   ) where
import Debug.Trace
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2009-04-08: Changed to {\tt ABR.\emph{Data}.BSTree}.


\submodule{BSTree type} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A \highlighttt{BSTree} is either empty or a node containing a key, an
associated value and left and right sub-trees. Type
{\tt key} must be an instance of type class
{\tt Ord}, so that {\tt <} and {\tt ==} work.
The slope of each node is stored at the node to avoid
recomputation.

\begin{code}
data BSTree key value =
     Empty
   | Node !key !value
          !(BSTree key value) !(BSTree key value) !Slope
   deriving (Eq, Ord, Show)
\end{code}

\noindent All the functions in this module maintain the following
invariant: The depth of left and right sub-trees differ
by no more than 1.
Associated with the left and right sub-trees is a slope
value which indicates the difference:
{\tt depth left $-$ depth right}.

\begin{code}
type Slope = Int
\end{code}

\noindent This returns the slope of a node.

\begin{code}
slope :: Ord k => BSTree k v -> Slope
slope (Node _ _ _ _ s) = s
\end{code}

\submodule{BSTree operations} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{emptyBST} is an empty {\tt BSTree}.

\begin{code}
emptyBST :: Ord k => BSTree k v
emptyBST = Empty
\end{code}

\noindent \highlighttt{nullBST}~$t$ returns {\tt True}
iff $t$ is empty.

\begin{code}
nullBST :: Ord k => BSTree k v -> Bool
nullBST Empty  = True
nullBST _      = False
\end{code}

\noindent \highlighttt{depthBST}~$t$ returns the depth of a $t$.
Finding depth is $O(\mathit{depth})$ because of the slope information.

\begin{code}
depthBST :: Ord k => BSTree k v -> Int
depthBST Empty   = 0
depth (Node _ _ l r s)
   | s >= 0      = 1 + depth l
   | otherwise   = 1 + depth r
\end{code}

\noindent After inserting or deleting a node the slope at a node
may be $-2$ or $2$. balance restores the $-1 \leq \mathit{slope} \leq 1$
invariant.

\begin{code}
balance :: Ord k => (BSTree k v, Int) -> (BSTree k v, Int)
balance (Node k v l r s, c)
   | 1 < s
      = (shiftRight (Node k v l r s), c - 1)
   | s < -1
      = (shiftLeft (Node k v l r s), c - 1)
   | otherwise
      = (Node k v l r s, c)
        where
        shiftRight (Node k v l r s)
           | slope l == -1
              = rotateRight (Node k v (rotateLeft l) r s)
           | otherwise
              = rotateRight (Node k v l r s)
        shiftLeft (Node k v l r s)
           | slope r == 1
              = rotateLeft (Node k v l (rotateRight r) s)
           | otherwise
              = rotateLeft (Node k v l r s)
        rotateRight (Node k v (Node k' v' l' r' s') r s)
           = let (ss, ss')
                    = case (s, s') of
                         ( 2,  0) -> (-1,  1)
                         ( 2,  1) -> ( 0,  0)
                         ( 2,  2) -> ( 0, -1)
                         ( 1,  1) -> (-1, -1)
                         ( 1,  0) -> (-1,  0)
                         ( 1, -1) -> (-2,  0)
             in Node k' v' l' (Node k v r' r ss') ss
        rotateLeft (Node k v l (Node k' v' l' r' s') s)
           = let (ss, ss')
                    = case (s, s') of
                         (-2,  0) -> ( 1, -1)
                         (-2, -1) -> ( 0,  0)
                         (-2, -2) -> ( 0,  1)
                         (-1, -1) -> ( 1,  1)
                         (-1,  0) -> ( 1,  0)
                         (-1,  1) -> ( 2,  0)
             in Node k' v' (Node k v l l' ss') r' ss
\end{code}

\noindent \highlighttt{updateBST}~$f~\mathit{key}~\mathit{value}~\mathit{bst}$ returns the
new tree obtained by updating $\mathit{bst}$ with the $\mathit{key}$ and $\mathit{value}$.
If the $\mathit{key}$ already exists, $f$ is used to combine the two
values. Use \verb"(\x _ -> x)" to merely replace.

\begin{code}
updateBST :: Ord k => (v -> v -> v) -> k -> v
             -> BSTree k v -> BSTree k v
updateBST f k' v'
   = fst . update
     where
     update Empty
        = (Node k' v' Empty Empty 0, 1)
     update (Node k v l r s)
        | k' < k
           = let (l', c') = update l
                 c = if s >= 0 && c' == 1 then 1 else 0
             in balance (Node k v l' r (s + c'), c)
        | k' == k
           = (Node k (f v' v) l r s, 0)
        | otherwise
           = let (r', c') =  update r
                 c = if s <= 0 && c' == 1 then 1 else 0
             in balance (Node k v l r' (s - c'), c)
\end{code}

\noindent \highlighttt{deleteBST}~$k~t$ returns the
new tree obtained by deleting the $k$ and its associated
value from $t$.

\begin{code}
deleteBST :: Ord k => k -> BSTree k v -> BSTree k v
deleteBST k'
   = fst . delete
     where
     delete Empty
        = (Empty, 0)
     delete (Node k v l r s)
        | k' < k
           = let (l', c') = delete l
                 c = if s == 1 && c' == -1 then -1 else 0
             in balance (Node k v l' r (s + c'), c)
        | k' == k
           = join l r s
        | otherwise
           = let (r', c') =  delete r
                 c = if s == -1 && c' == -1 then -1 else 0
             in balance (Node k v l r' (s - c'), c)
     join Empty r _
        = (r, -1)
     join l r s
        = let ((l', c'), k', v') = split l
              s' = s + c'
              c = if s == 1 && c' == -1 then -1 else 0
          in balance (Node k' v' l' r s', c)
     split (Node k v l Empty s)
        = ((l, -1), k, v)
     split (Node k v l r s)
        = let ((r', c'), k', v') = split r
              c = if s == -1 && c' == -1 then -1 else 0
          in (balance (Node k v l r' (s - c'), c), k', v')
\end{code}

\noindent \highlighttt{lookupBST}~$k~t$ returns
{\tt Just}~$v$, where $v$ is the value associated
with $k$ in $t$, or {\tt Nothing}.

\begin{code}
lookupBST :: Ord k => k -> BSTree k v -> Maybe v
lookupBST k Empty
   = Nothing
  --  ``Searches through a binary tree 
lookupBST k' (Node k v l r _)
   | k' < k    = trace("In BSTree.lhs lookupBST line 217 --> k' < k ")lookupBST k' l
   | k' == k   = trace("In BSTree.lhs lookupBST line 217 --> k' == k ") Just v
   | otherwise = trace("In BSTree.lhs lookupBST line 217 --> otherwise") lookupBST k' r
\end{code}

\noindent \highlighttt{memberBST}~$k~t$ returns
{\tt True} iff $k$ occurs in $t$.

\begin{code}
memberBST :: Ord k => k -> BSTree k v -> Bool
memberBST k t
   = case lookupBST k t of
        Nothing -> False
        Just _  -> True
\end{code}

\noindent \highlighttt{lookupGuard}~$\mathit{bst}~\mathit{keys}~\mathit{handler}~\mathit{process}$
tries to look up the $\mathit{keys}$. If any are missing the $\mathit{handler}$
is applied to the first missing key otherwise the $\mathit{process}$
is applied to the list of values successfully looked up.

\begin{code}
lookupGuard :: Ord a => BSTree a b -> [a] -> (a -> c)
                        -> ([b] -> c) -> c
lookupGuard bst keys handler process
   = lookupGuard' keys []
     where
     lookupGuard' [] vals
        = process vals
     lookupGuard' (k:ks) vals
        = case lookupBST k bst of
             Nothing    ->
	        handler k
             Just stuff ->
	        lookupGuard' ks (vals ++ [stuff])
\end{code}

\noindent \highlighttt{flattenBST}~$t$ returns the list of tuples $(k,v)$
in $t$ in ascending order of key.

\begin{code}
flattenBST :: Ord k => BSTree k v -> [(k,v)]
flattenBST Empty
   = []
flattenBST (Node k v Empty r _)
   = (k, v) : flattenBST r
flattenBST (Node k v (Node k' v' l' r' _) r _)
   = flattenBST (Node k' v' l' (Node k v r' r 0) 0)
\end{code}

\noindent \highlighttt{domBST}~$t$ returns the list
of keys in $t$ in ascending order of key.

\begin{code}
domBST :: Ord k => BSTree k v -> [k]
domBST Empty
   = []
domBST (Node k _ Empty r _)
   = k : domBST r
domBST (Node k v (Node k' v' l' r' _) r _)
   = domBST (Node k' v' l' (Node k v r' r 0) 0)
\end{code}

\noindent \highlighttt{domBST}~$t$ returns the list
of values in $t$ in ascending order of key.

\begin{code}
ranBST :: Ord k => BSTree k v -> [v]
ranBST Empty
   = []
ranBST (Node k v Empty r _)
   = v : ranBST r
ranBST (Node k v (Node k' v' l' r' _) r _)
   = ranBST (Node k' v' l' (Node k v r' r 0) 0)
\end{code}

\noindent \highlighttt{pairs2BST}~$\mathit{kvs}$ converts an
association list $\mathit{kvs}$ of pairs $(k,v)$ to a {\tt
BSTree}. If there are duplicate $v$'s for a $k$,
only the first is retained.

\begin{code}
pairs2BST :: Ord k => [(k,v)] -> BSTree k v
pairs2BST []
   = Empty
pairs2BST ((k,v):kvs)
   = updateBST (\x _ -> x) k v (pairs2BST kvs)
\end{code}

\noindent \highlighttt{list2BST}~$\mathit{ks}~v$ converts a
list of keys $\mathit{ks}$ to a {\tt BSTree}. The values in
the tree are all assigned $v$.

\begin{code}
list2BST :: Ord k => [k] -> v -> BSTree k v
list2BST [] _
   = Empty
list2BST (k:ks) v
   = updateBST (\x _ -> x) k v (list2BST ks v)
\end{code}

\noindent \highlighttt{countBST}~$t$ returns the
number of elements in $t$.

\begin{code}
countBST :: Ord k => BSTree k v -> Int
countBST Empty            = 0
countBST (Node _ _ l r _) = countBST l + 1 + countBST r
\end{code}

\noindent \highlighttt{leftBST}~$t$ returns the
left-most element of $t$.
\highlighttt{rightBST}~$t$ returns the right-most
element of $t$.

\begin{code}
leftBST, rightBST :: Ord k => BSTree k v -> Maybe (k, v)
leftBST Empty                = Nothing
leftBST (Node k v Empty _ _) = Just (k, v)
leftBST (Node k v l _ _)     = leftBST l
rightBST Empty                = Nothing
rightBST (Node k v _ Empty _) = Just (k, v)
rightBST (Node k v _ r _)     = rightBST r
\end{code}

\noindent \highlighttt{mapBST}~$f~t$ returns the
tree formed by applying $f$ to all of the values in
$t$. The keys are not changed.

\begin{code}
mapBST :: Ord k => (v -> v') -> BSTree k v -> BSTree k v'
mapBST _ Empty
   = Empty
mapBST f (Node k v l r s)
   = Node k (f v) (mapBST f l) (mapBST f r) s
\end{code}
