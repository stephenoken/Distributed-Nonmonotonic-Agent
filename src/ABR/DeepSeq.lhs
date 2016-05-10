% DeepSeq.lhs
% This file was produced from DeepSeq.lit

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

\module{DeepSeq} %%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.DeepSeq} was pinched from Dean Herington, who
says:

\begin{quote}
   ``The prelude support for strict evaluation,
   \verb"seq" and \verb"($!)", evaluate only enough
   to ensure that the value being forced is not
   bottom.  In your case you need a deeper
   evaluation to be forced.

   ``A clean (though somewhat tedious) way to
   achieve what you need is with the deepSeq
   function from the following module.

   ``The {\tt DeepSeq} class provides a method {\tt
   deepSeq} that is similar to {\tt seq} except
   that it forces deep evaluation of its first
   argument before returning its second argument.

   ``Instances of {\tt DeepSeq} are provided for
   Prelude types.  Other instances must be supplied
   by users of this module.''
\end{quote}

\begin{code}
module ABR.DeepSeq (DeepSeq(..), ($!!)) where
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Requires review.


\submodule{Class Definition} %%%%%%%%%%%%%%%%%

\noindent \highlighttt{DeepSeq} has only one
method, \highlighttt{deepSeq}~$x~y$ deeply
evaluates $x$ and then returns $y$.

\begin{code}
class  DeepSeq a where
\end{code}

\begin{code}
   deepSeq :: a -> b -> b
   deepSeq = seq      -- default, for simple cases
\end{code}

\submodule{Infix operator} %%%%%%%%%%%%%%%%%

\noindent $f$~\highlightttNoindex{\$!!}\indextt{\$"!"!}~$x$ deeply
evaluates $x$ and then returns $f~x$.

\begin{code}
infixr 0 `deepSeq`, $!!
\end{code}

\begin{code}
($!!) :: (DeepSeq a) => (a -> b) -> a -> b
f $!! x = x `deepSeq` f x
\end{code}

\submodule{Instance Declarations} %%%%%%%%%%%%%%%%%

\subsubmodule{Simple instances}

\begin{code}
instance DeepSeq ()       where {}
instance DeepSeq Bool     where {}
instance DeepSeq Char     where {}
instance DeepSeq Ordering where {}
instance DeepSeq Integer  where {}
instance DeepSeq Int      where {}
instance DeepSeq Float    where {}
instance DeepSeq Double   where {}
\end{code}

\subsubmodule{Tuple instances}

\begin{code}
instance (DeepSeq a, DeepSeq b) =>
         DeepSeq (a,b) where
   deepSeq (a,b) y = deepSeq a $ deepSeq b y
\end{code}

\begin{code}
instance (DeepSeq a, DeepSeq b, DeepSeq c) =>
      DeepSeq (a,b,c) where
   deepSeq (a,b,c) y = deepSeq a $ deepSeq b $ deepSeq c y
\end{code}

\begin{code}
instance (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d) =>
      DeepSeq (a,b,c,d) where
   deepSeq (a,b,c,d) y = deepSeq a $ deepSeq b $
      deepSeq c $ deepSeq d y
\end{code}

\begin{code}
instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d,
      DeepSeq e) => DeepSeq (a,b,c,d,e) where
   deepSeq (a,b,c,d,e) y = deepSeq a $ deepSeq b $
      deepSeq c $ deepSeq d $ deepSeq e y
\end{code}

\begin{code}
instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d,
      DeepSeq e, DeepSeq f) => DeepSeq (a,b,c,d,e,f) where
   deepSeq (a,b,c,d,e,f) y = deepSeq a $ deepSeq b $
      deepSeq c $ deepSeq d $ deepSeq e $ deepSeq f y
\end{code}

\begin{code}
instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d,
      DeepSeq e,DeepSeq f,DeepSeq g) =>
      DeepSeq (a,b,c,d,e,f,g) where
   deepSeq (a,b,c,d,e,f,g) y = deepSeq a $ deepSeq b $
      deepSeq c $ deepSeq d $ deepSeq e $ deepSeq f $
      deepSeq g y
\end{code}

\subsubmodule{List instance}

\begin{code}
instance (DeepSeq a) => DeepSeq [a] where
   deepSeq [] y = y
   deepSeq (x:xs) y = deepSeq x $ deepSeq xs y
\end{code}

\subsubmodule{Maybe instance}

\begin{code}
instance (DeepSeq a) => DeepSeq (Maybe a) where
   deepSeq Nothing y = y
   deepSeq (Just x) y = deepSeq x y
\end{code}

\subsubmodule{Either instance}

\begin{code}
instance (DeepSeq a, DeepSeq b) =>
      DeepSeq (Either a b) where
   deepSeq (Left a) y = deepSeq a y
   deepSeq (Right b) y = deepSeq b y
\end{code}
