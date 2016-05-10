% List.lhs
% This file was produced from List.lit

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

\module{Control.List} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Control.List} implements control 
abstractions involving lists.

\begin{code}
module ABR.Control.List (pam) where
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

New 2009-04-08: Separated from {\tt ABR.List}.
   

\submodule{Backwards map} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{pam}~$\mathit{fs}~x$ returns the list of results obtained
by applying all the functions in $\mathit{fs}$ to $x$.

\begin{code}
pam :: [a -> b] -> a -> [b]
pam fs x = [f x | f <- fs]
\end{code}

