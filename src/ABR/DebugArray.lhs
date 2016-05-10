% DebugArray.lhs
% This file was produced from DebugArray.lit

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

\module{DebugArray} %%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.DebugArray} is used to help
debug programs that use arrays.

\begin{code}
module ABR.DebugArray (array', accumArray', (!!!)) where
\end{code}

\begin{code}
import Data.Array
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Requires review.
   

\submodule{Functions} %%%%%%%%%%%%%%%%%%%%%%

\noindent A
\highlightttNoindex{!!!}\index{\BANG\BANG\BANG@{\tt
\BANG\BANG\BANG}} is a replacement for {\tt !} that
displays a different error message so you can pin down
which array indexing operation is out of range.

\begin{code}
(!!!) :: (Ix i, Show i, Show e) => Array i e -> i -> e
a !!! i = 
   let b = bounds a
   in if inRange b i
   then a ! i
   else error $
      "ABR.DebugArray.!!! Array index out of bounds:\n"
      ++ "bounds = " ++ show b ++ "\n" 
      ++ "index = " ++ show i ++ "\n"
      ++ "array = " ++ show a ++ "\n"
\end{code}

\noindent A \highlighttt{array'} is a replacement for {\tt array}
that displays a different error message so you can pin
down which array indexing operation is out of range.

\begin{code}
array' :: (Ix i, Show i, Show e) => 
   (i,i) -> [(i,e)] -> Array i e
array' b ies = 
  case [i | (i,e) <- ies, not (inRange b i)] of
     [] -> array b ies
     is -> error $
      "ABR.array'.!!! Array indices out of bounds:\n"
      ++ "bounds = " ++ show b ++ "\n" 
      ++ "indices = " ++ show is ++ "\n"
      ++ "pairs = " ++ show ies ++ "\n"
\end{code}

\noindent A \highlighttt{accumArray'} is a replacement for {\tt accumArray}
that displays a different error message so you can pin
down which array indexing operation is out of range.

\begin{code}
accumArray' :: (Ix i, Show i, Show a) => (e -> a -> e) 
   -> e -> (i,i) -> [(i,a)] -> Array i e
accumArray' f e b ies = 
  case [i | (i,e) <- ies, not (inRange b i)] of
     [] -> accumArray f e b ies
     is -> error $
      "ABR.accumArray'.!!! Array indices out of bounds:\n"
      ++ "bounds = " ++ show b ++ "\n" 
      ++ "indices = " ++ show is ++ "\n"
      ++ "pairs = " ++ show ies ++ "\n"
\end{code}

