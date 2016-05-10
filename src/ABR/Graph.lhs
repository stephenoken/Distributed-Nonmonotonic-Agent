% Graph.lhs
% This file was produced from Graph.lit

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

\module{Graphs} %%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Graphs} implements a directed,
unweighted graph as an ADT. This is \emph{UNDER CONSTRUCTION}.
This implementation closely follows that described
by Rabhi and Lapalme~\cite{rabhi:99} and
Launchbury~\cite{Launchbury:95}.

\begin{code}
module ABR.Graph (
      Graph(..), SGraph, mapG, transposeG
      -- isReachable, reachable, isCyclic
   ) where
\end{code}

\begin{code}
import Data.Array; import Control.Monad.ST.Lazy
\end{code}

\begin{code}
import ABR.SparseSet
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Requires review.


\submodule{Graph abstract data type} %%%%%%%%%%%%%%%%%%%%%

A \emph{vertex} is a value from some enumerated type
and for implementation reasons must usually be an
instance of classes {\tt Eq}, {\tt Ord}, {\tt Ix}
and {\tt Show}.

An \emph{edge} is an ordered association two vertices.
Note that we are only dealing with unweighted graphs
here.

\begin{code}
type Edge v = (v,v)
\end{code}

A graph $G = (V, E)$ consists of a set of edges $E$ that
connect a set of vertices $V$. The set of edges $E$ is a
relation on $V$. If $G$ is a directed graph, $E$ is not
symmetric. The number of vertices is $|V|$ and the number
of edges is $|E|$.

A graph, as an abstract data type is defined by the
methods of this type class.

\begin{code}
class Graph g where
\end{code}

\noindent {\tt mkGraph}~$v$~$v'$~$E$ builds a graph
$(V, E)$. The set of vertices $V$ assumed to exist is the
range $[v..v']$. $E$ is the relation defining the edges.

\begin{code}
   mkGraph :: Ix v => v -> v -> [Edge v] -> g v
\end{code}

\noindent {\tt vertices}~$G$ returns the list of vertices
$V$ in graph $G$.

\begin{code}
   vertices :: Ix v => g v -> [v]
\end{code}

\noindent {\tt boundsG}~$G$ returns the least and greatest
of the vertices $V$ in graph $G$.

\begin{code}
   boundsG :: Ix v => g v -> (v,v)
\end{code}

\noindent {\tt edges}~$G$ returns the list of edges $E$
in graph $G$.

\begin{code}
   edges :: Ix v => g v -> [Edge v]
\end{code}

\noindent {\tt adjacent}~$G$~$v$ returns the list of
vertices in graph $G$ that can be reached from vertex $v$
in one step.

\begin{code}
   adjacent :: Ix v => g v -> v -> [v]
\end{code}

\noindent {\tt isAdjacent}~$G$~$v$~$v'$ returns {\tt True}
iff vertex $v'$ in graph $G$ can be reached from vertex
$v$ in one step.

\begin{code}
   isAdjacent :: Ix v => g v -> v -> v -> Bool
\end{code}

%%%%%%%
%%%%%%%%%%%%%%%%
%%%%%%%





\submodule{Sparse graph type} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

There are several ways to represent graphs as a Haskell
data structure. The following is likely to be suitable
when the graph is sparse.

\begin{code}
newtype SGraph vertex =
   SGraph (Array vertex (SparseSet vertex))
   deriving (Show)
\end{code}

\noindent This uses an array to map each vertex $v$ in $V$
to the set of vertices reachable from $v$ in one step.

\begin{code}
instance Graph SGraph where
\end{code}

\begin{code}
   mkGraph v v' vvs = SGraph $
      accumArray (flip insertSS) emptySS (v,v') vvs
\end{code}

\begin{code}
   vertices (SGraph g) = indices g
\end{code}

\begin{code}
   boundsG (SGraph g) = bounds g
\end{code}

\begin{code}
   edges (SGraph g) =
      [(v,v') | v <- indices g, v' <- flattenSS (g!v)]
\end{code}

\begin{code}
   adjacent (SGraph g) v = flattenSS (g ! v)
\end{code}

\begin{code}
   isAdjacent (SGraph g) v v' = v' `elemSS` (g ! v)
\end{code}


%%%%%%%
%%%%%%%%%%%%%%%%
%%%%%%%



\submodule{Example sparse graphs} %%%%%%%%%%%%%%%%%%%%%%%%

{\tt ga} is a non-cyclic graph.

\begin{code}
ga :: SGraph Int
ga = mkGraph 0 9 [
      (0,1), (0,3),
      (1, 2),
      (3, 4), (3,5), (3, 6), (3,1)
   ]
\end{code}

{\tt gb} is a cyclic graph.

\begin{code}
gb :: SGraph Int
gb = mkGraph 0 9 [
      (0,1), (0,3),
      (1, 2), (1,0),
      (3, 4), (3,5), (3, 6), (3,1)
   ]
\end{code}


\submodule{Graph Operations} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt mapG}~$f$~$v$~$v'$~$G$ builds a new graph $G'$
formed by applying $f$ to every edge in $G$, such that:
\begin{enumerate}
   \item $G  = (V , E )$
   \item $G' = (V', E')$
   \item $V' = [v..v']$
   \item $E' = \{f\,e | e \in E\}$
\end{enumerate}

\begin{code}
mapG :: (Ix v, Ix v', Graph g, Graph g') =>
   (Edge v -> Edge v') -> v' -> v' -> g v -> g' v'
mapG f v v' = mkGraph v v' . map f . edges
\end{code}

\noindent {\tt transposeG}~$G$ reverses all the edges in
$G$.

\begin{code}
transposeG :: (Ix v, Graph g) => g v -> g v
transposeG g =
   let (v,v') = boundsG g
   in mapG (\(v,v') -> (v',v)) v v' g
\end{code}



\submodule{Reachability} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt isReachable g v v'} returns {\tt True} iff a vertex
{\tt v'} in graph {\tt g} is reachable from vertex {\tt v}.
A depth-first search is used. This implementation uses
a mutable array so that already visited nodes can be
skipped in constant time.

\begin{haddock}
isReachable :: (Ix v) => Graph v -> v -> v -> Bool
isReachable g v v' = runST (do
      visited <- newSTArray (bounds g) False
      let ir toVisit = case toVisit of
             []     -> return False
	     (v:vs) -> if v == v'
 	        then return True
	        else do
		   vv <- readSTArray visited v
		   if vv
		      then ir vs
		      else do
		         writeSTArray visited v True
			 ir (adjacent g v ++ vs)
      ir (adjacent g v)
   )
\end{haddock}

{\tt reachable g v} returns the list of vertices in
graph {\tt g} that are reachable from vertex
{\tt v'}. A depth-first search is used. This
implementation uses a mutable array so that already
visited nodes can be skipped in constant time.

\begin{haddock}
reachable :: (Ix v) => Graph v -> v -> [v]
reachable g v = runST (do
      visited <- newSTArray (bounds g) False
      let r toVisit rvs = case toVisit of
             []     -> return rvs
	     (v:vs) -> do
                vv <- readSTArray visited v
                if vv
		   then r vs rvs
		   else do
		      writeSTArray visited v True
		      r (adjacent g v ++ vs) (v : rvs)
      r (adjacent g v) []
   )
\end{haddock}


\submodule{Cycles detection} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt isCyclic g} returns {\tt True} iff graph {\tt g} is
cyclic. A depth-first search is used.

\begin{haddock}
isCyclic :: (Ix v, Enum v) => Graph v -> Bool
isCyclic g
   = let (lo,hi) = bounds g
     in or [isReachable g v v | v <- [lo..hi]]
\end{haddock}


\submodule{Dense graph type} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

When the
graph is dense a representation using an array
of arrays may be more suitable.


\submodule{Imported Stuff} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{Verbatim}[frame=leftline]

mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (Array.bounds t) [(v, f v (t ! v)) | v <- Array.indices t]

type Bounds = (Vertex, Vertex)

outdegree :: Graph -> Table Int
outdegree g = mapT numEdges g
	where
		numEdges v ws = length ws

buildG :: Bounds -> [Edge] -> Graph
buildG bnds es = accumArray (flip (:)) [] bnds es

transposeG :: Graph -> Graph
transposeG g = buildG (Array.bounds g) (reverseE g)

reverseE :: Graph -> [Edge]
reverseE g = [ (w,v) | (v,w) <- edges g]

indegree :: Graph -> Table Int
indegree g = outdegree (transposeG g)

---------------------------------------------------------
-- TREE
---------------------------------------------------------

data Tree a = Node a (Forest a) deriving Show
type Forest a = [Tree a]

---------------------------------------------------------
-- Depth First Numbering
---------------------------------------------------------
preorder :: Tree a -> [a]
preorder (Node a ts) = [a] ++ preorderF ts

preorderF :: Forest a -> [a]
preorderF ts = concat (map preorder ts)

preOrd :: Graph -> [Vertex]
preOrd g = preorderF (dff g)

tabulate :: Bounds -> [Vertex] -> Table Int
tabulate bnds vs = array bnds (zip vs [1..])

preArr :: Bounds -> Forest Vertex -> Table Int
preArr bnds ts = tabulate bnds (preorderF ts)

---------------------------------------------------------
-- Topological Sorting
---------------------------------------------------------
postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

postorderF :: Forest a -> [a]
postorderF ts = concat (map postorder ts)

postOrd :: Graph -> [Vertex]
postOrd g = postorderF (dff g)

topSort :: Graph -> [Vertex]
topSort g = reverse (postOrd g)

postArr bnds ts = tabulate bnds (postorderF ts)

---------------------------------------------------------
-- Connected Components
---------------------------------------------------------

components :: Graph -> Forest Vertex
components g = dff (undirected g)

undirected :: Graph -> Graph
undirected g = buildG (Array.bounds g) (edges g ++ reverseE g)

---------------------------------------------------------
-- Strongly connected components
---------------------------------------------------------

scc :: Graph -> Forest Vertex
scc g = dfs (transposeG g) (reverse (postOrd g))

scc' :: Graph -> Forest Vertex
scc' g = dfs g (reverse (postOrd (transposeG g)))

---------------------------------------------------------
-- Depth First Search Algorithms
---------------------------------------------------------

dfs :: Graph -> [Vertex] -> Forest Vertex
dfs g vs = prune (Array.bounds g) (map (generate g) vs)

dff :: Graph -> Forest Vertex
dff g = dfs g (vertices g)

generate :: Graph -> Vertex -> Tree Vertex
generate g v = Node v (map (generate g) (g ! v))

type Set s = STArray s Vertex Bool

mkEmpty :: Bounds -> ST s (Set s)
mkEmpty bnds = newArray bnds False

contains :: Set s -> Vertex -> ST s Bool
contains m v = readArray m v

include :: Set s -> Vertex -> ST s ()
include m v = writeArray m v True

prune :: Bounds -> Forest Vertex -> Forest Vertex
prune bnds ts = runST (mkEmpty bnds >>= \m -> chop m ts)

chop :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop m [] = return []
chop m (Node v ts : us) = contains m v >>= \visited ->
			if visited then
				chop m us
			else
				include m v	>>
				chop m ts	>>= \as ->
				chop m us	>>= \bs ->
				return ((Node v as) : bs)

---------------------------------------------------------
-- Classification of graph edges
---------------------------------------------------------

tree :: Bounds -> Forest Vertex -> Graph
tree bnds ts = buildG bnds (concat (map flat ts))
	where
		flat (Node v ts) = [ (v,w) | Node w us <- ts ] ++ concat (map flat ts)

back :: Graph -> Table Int -> Graph
back g post = mapT select g
	where
		select v ws = [ w | w <- ws, post!v < post!w ]

cross :: Graph -> Table Int -> Table Int -> Graph
cross g pre post = mapT select g
	where
		select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward :: Graph -> Graph -> Table Int -> Graph
forward g tree pre = mapT select g
	where
		select v ws = [ w | w <- ws, pre!v < pre!w, w `notElem` tree ! v]
--		select v ws = [ w | w <- ws, pre!v < pre!w] \\ tree ! v

---------------------------------------------------------
-- All Paths
---------------------------------------------------------

type Path = [Vertex]
-- allpaths :: Graph -> Vertex -> Vertex -> Path

---------------------------------------------------------
-- Main Routine
---------------------------------------------------------

makeFromTo h l = hIsEOF h 			>>= \b ->
		if b then return l else
			-- hGetLine h 				>>= \(f:_:t:_) ->
			hGetChar h 				>>= \f ->	-- from
			hGetChar h 				>>= \_ ->	-- ' '
			hGetChar h 				>>= \t ->	-- to
			consumetherest h		>>
--			hGetChar h 				>>			-- '\n'
			makeFromTo h (l ++ [(f,t)])
	where
		consumetherest h = hIsEOF h >>= \b ->
			if b then return []
			else hGetChar h >>= \t ->
				if t == '\n' then return [] else consumetherest h

readGraph f = if f == "" then makeFromTo stdin []
		else openFile f ReadMode	>>= \h -> makeFromTo h []

execute "-pre-order" f = readGraph f >>= \((f,t):xs) ->
		let g = buildG (f,t) xs in print (preArr (f,t) (dfs g [f..t]))
execute "-post-order" f = readGraph f >>= \((f,t):xs) ->
		let g = buildG (f,t) xs in print (postArr (f,t) (dfs g [f..t]))
execute "-scc" f = readGraph f >>= \((f,t):xs) ->
		let g = buildG (f,t) xs in print (scc g)
execute "-forward-edge" f = readGraph f >>= \((f,t):xs) ->
		let
			g       = buildG (f,t) xs
			dfstree = dfs g [f..t]
		in
			print (forward g (tree (f,t) dfstree) (preArr (f,t) dfstree))
execute "-tree-edge" f = readGraph f >>= \((f,t):xs) ->
		let g = buildG (f,t) xs
		in
			print (tree (f,t) (dfs g [f..t]))
execute "-back-edge" f = readGraph f >>= \((f,t):xs) ->
		let g = buildG (f,t) xs
		in print (back g (postArr (f,t) (dfs g [f..t])))
execute "-cross-edge" f = readGraph f >>= \((f,t):xs) ->
		let
			g       = buildG (f,t) xs
			dfstree = dfs g [f..t]
		in print (cross g (preArr (f,t) dfstree) (postArr (f,t) dfstree))

execute a _ = putStr "I don't know the option, "	>>
		putStr a								>>
		putStr "\n"								>>
		printUsage

printUsage = getProgName	>>= \pname ->
		putStr pname		>>
		putStr " ["			>>
		putStr "-pre-order"	>>
		putStr "| -post-order"	>>
		putStr "| -scc"	>>
		putStr "| -forward-edge" >>
		putStr "| -back-edge" >>
		putStr "| -tree-edge" >>
		putStr "| -cross-edge" >>
		putStr "| -all-cycles" >>
		putStr "] "			>>
		putStr "file"		>>
		putStr "\n"

main = getArgs >>= \args ->
	case args of
		[] 			-> printUsage
		(a:[])		-> execute a ""
		(a:f:_)		-> execute a f

ex1 = buildG ('a', 'j') [
	('a', 'j'),
	('a', 'g'),
	('b', 'i'),
	('b', 'a'),
	('c', 'h'),
	('c', 'e'),
	('e', 'j'),
	('e', 'h'),
	('e', 'd'),
	('f', 'i'),
	('g', 'f'),
	('g', 'b')]
\end{Verbatim}
