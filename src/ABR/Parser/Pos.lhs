% Pos.lhs
% This file was produced from Parser.lit

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

\module{Parser.Pos} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\label{parserModule}

The \highlighttt{ABR.Parser.Pos} defines a type for describing the
position in a source code.

\begin{code}
module ABR.Parser.Pos (Line, Col, Pos, HasPos(..)) where
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2009-04-13: Split from {\tt ABR.Parser}.


\submodule{Positions in a source} %%%%%%%%%%%%%%%%%%%%%%%%

To report error the position, \highlighttt{Pos}, of
a character or token in a source is required. The
first line and column are indicated with
\highlighttt{Line} and \highlighttt{Col} values of
0. A negative {\tt Line} value indicates ``Don't
know where''.

\begin{code}
type Line = Int
type Col  = Int
type Pos  = (Line, Col)
\end{code}

\noindent Positions get embedded in all kinds of
parse tree types. Having one overloaded function that projects
out a {\tt Pos} is useful. Make parse tree types with
postions in them an instance of \highlighttt{HasPos}.

\begin{code}
class HasPos a where
\end{code}

\noindent \highlighttt{getPos}~$x$ returns the position
of $x$. \highlighttt{pos} is a legacy alias.

\begin{code}
   getPos, pos :: a -> Pos
   getPos = error "undefined HasPos instance"
   pos = getPos
\end{code}

\subsubmodule{Container intances}

\begin{code}
instance (HasPos a, HasPos b) => HasPos (Either a b) where
   getPos e = case e of
      Left x  -> getPos x
      Right x -> getPos x
\end{code}
