% Queue.lhs
% This file was produced from Queue.lit

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

\module{Data.Queue} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{ABR.Data.Queue} module implements the Queue ADT.

\begin{code}
module ABR.Data.Queue (
      Queue, emptyQ, isEmptyQ, attachQ, frontQ, detachQ,
      extractQ
   ) where
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2009-04-08: Changed to {\tt ABR.\emph{Data}.Queue}.
   
Reviewed: 2009-04-01. There's nothing in the standard
libraries that really does this. There are more compicated
queue pakages in Hackage, but no compeling reason to drop 
this library for them. Possible extensions: maybe some
instances like {\tt Traversable}, but as needed.
   

\submodule{Data type} %%%%%%%%%%%%%%%%

A \highlighttt{Queue} is a first-in-first-out sequence.

\begin{code}
type Queue a = 
   ([a],[a])
\end{code}

\noindent A queue is a tuple of $(\mathit{fronts}, \mathit{backs})$ where:

\begin{description}
   \item{$\mathit{fronts}$} is a list of foremost elements
      in order; and
   \item{$\mathit{backs}$} is a list if the hindmost elements
      in reverse order.
\end{description}

\noindent For efficiency, we maintain
the invariant: a queue is empty or $\mathit{fronts}$ is not empty.

\submodule{Operations} %%%%%%%%%%%%%%%%%%%%%%
   
\noindent \highlighttt{emptyQ} is an empty queue.

\begin{code}
emptyQ :: Queue a
emptyQ = ([],[])
\end{code}

\noindent \highlighttt{isEmptyQ}~$q$ returns {\tt True} iff queue
$q$ is empty.

\begin{code}
isEmptyQ :: Queue a -> Bool
isEmptyQ ([],_) = True
isEmptyQ _      = False
\end{code}

\noindent \highlighttt{attachQ}~$e~q$ attaches $e$ to the back
of queue $q$.   

\begin{code}
attachQ :: a -> Queue a -> Queue a
attachQ e ([],_)          = ([e], [])
attachQ e (fronts, backs) = (fronts, e : backs)
\end{code}

\noindent \highlighttt{frontQ}~$q$ returns the value at the front
of queue $q$.

\begin{code}
frontQ :: Queue a -> a
frontQ ([],_)      = 
   error "Can't get frontQ of an empty queue."
frontQ (front:_,_) = front
\end{code}

\noindent \highlighttt{detachQ}~$q$ returns the element that was
at the front of queue $q$ and the $q$ after that element
has been detached. 

\begin{code}
detachQ :: Queue a -> (a, Queue a)
detachQ ([],_)               = 
   error "Can't detachQ an empty queue."
detachQ ([front], backs)     = (front, (reverse backs, []))
detachQ (front:fronts,backs) = (front, (fronts, backs))
\end{code}

\noindent \highlighttt{extractQ}~$q$ returns the list of all
elements in queue $q$.

\begin{code}
extractQ :: Queue a -> [a]
extractQ ([],_) = []
extractQ q      = front : extractQ rest
   where
   (front, rest) = detachQ q
\end{code}
