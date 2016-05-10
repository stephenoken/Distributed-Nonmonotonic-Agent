% Args.lhs
% This file was produced from Args.lit

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

\module{Args: Command Line Arguments} %%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Args} provides a way to pick apart the
meanings of command line arguments.

\begin{code}
module ABR.Args (
      OptSpec(..), OptVal(..), Options, findOpts,
      lookupFlag, lookupParam, lookupQueue
   ) where
\end{code}

\begin{code}
import ABR.Data.BSTree; import ABR.Data.Queue
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Requires review.
   

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An \highlighttt{OptSpec} is used to specify the
types of option expected. 

\begin{code}
data OptSpec = 
   FlagS String | ParamS String | QueueS String
   deriving (Eq, Show)
\end{code}

\noindent An option is one of:

\begin{itemize}
   \item a flag to be set or unset. Specify with 
      \highlighttt{FlagS}~$\mathit{name}$. Users
      set or unset with \verb"+"$\mathit{name}$ or
      \verb"-"$\mathit{name}$ respectively.
   \item a parameter with a value. Specify with
      \highlighttt{ParamS}~$\mathit{name}$. Users
      provide values with
      \verb"-"$\mathit{name}~\mathit{value}$.
   \item a parameter that can have multiple values.
      The order of the multiple values might be
      significant. In this case a queue of strings
      should be returned. Specify with
      \highlighttt{QueueS}~$\mathit{name}$. Users
      provide values with
      \verb"-"$\mathit{name}~\mathit{value}_{1}$
      \verb"-"$\mathit{name}~\mathit{value}_{2}~\ldots$.
\end{itemize}   

\noindent An \highlighttt{OptVal} is used to
indicate presence of a command line option. Flags
might be \highlighttt{FlagPlus} or
\highlighttt{FlagMinus}. Parameters will either
return the
\highlighttt{ParamValue}~$\mathit{value}$ or an
indication that the value was missing,
\highlighttt{ParamMissingValue}. Queue parameters
return \highlighttt{ParamQueue}~$\mathit{queue}$.
Missing values for queue parameters might yield an
empty queue.

\begin{code}
data OptVal = 
      FlagPlus | FlagMinus | 
      ParamValue String | ParamMissingValue |
      ParamQueue (Queue String)
   deriving Show
\end{code}

\noindent An \highlighttt{Options} is used to map
from an option name to its value(s).

\begin{code}
type Options = BSTree String OptVal
\end{code}


\submodule{Option detection} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{findOpts}~$\mathit{optSpecs}~\mathit{
args}$ returns $(\mathit{options},
\mathit{leftovers})$, where:\\
$\mathit{optSpecs}$ is
a list of option specifications; $\mathit{args}$ is
a list of command line arguments;
$\mathit{options}$ is a BSTree mapping the option
names to the values found; and $\mathit{leftovers}$
is a list of any unconsumed arguments, typically
file names.
      
\begin{code}
findOpts :: [OptSpec] -> [String] -> (Options, [String])
findOpts specs = fo
   where
   rep, enq :: Options -> String -> OptVal -> Options
   rep t cs v = updateBST (\x _ -> x) cs v t
   enq t cs v = updateBST enq' cs v t
   enq' :: OptVal -> OptVal -> OptVal
   enq' (ParamQueue q) (ParamQueue q') = if isEmptyQ q 
      then ParamQueue q'
      else ParamQueue (attachQ (frontQ q) q')
   fo :: [String] -> (Options, [String])
   fo args = case args of
      [] -> (emptyBST, [])
      ((c:cs):css) ->
         if c == '+' && FlagS cs `elem` specs then
	    let (t, css') = fo css
	    in (rep t cs FlagPlus, css')
	 else if c == '-' && FlagS cs `elem` specs then
	    let (t, css') = fo css
	    in (rep t cs FlagMinus, css')
	 else if c == '-' && ParamS cs `elem` specs then
	    case css of
	       [] ->
	          (rep emptyBST cs ParamMissingValue, [])
	       (cs':css') ->
	          let (t, css'') = fo css'
	          in (rep t cs (ParamValue cs'), css'')
	 else if c == '-' && QueueS cs `elem` specs then
	    case css of
	       [] ->
	          (enq emptyBST cs (ParamQueue emptyQ), [])
	       (cs':css') ->
	          let (t, css'') = fo css'
	          in (enq t cs (ParamQueue
	                (attachQ cs' emptyQ)), css'')
         else 
	    let (t, css') = fo css
	    in (t, (c:cs):css')
\end{code}

\submodule{Looking up options} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{lookupFlag}~$\mathit{name}~\mathit{options}~\mathit{def}$
returns the value ({\tt FlagPlus} means {\tt True}) stored
for the $\mathit{name}$ed flag in $\mathit{options}$ or
$\mathit{def}$ if it has not been properly specified.
      
\begin{code}
lookupFlag :: String -> Options -> Bool -> Bool
lookupFlag name options def = 
   case lookupBST name options of
      Just FlagPlus  -> True
      Just FlagMinus -> False
      _              -> def
\end{code}

\noindent
\highlighttt{lookupParam}~$\mathit{name}~\mathit{options}~\mathit{def}$
returns the value stored for the $\mathit{name}$ed
parameter in $\mathit{options}$ or $\mathit{def}$ if it
has not been properly specified.
      
\begin{code}
lookupParam :: String -> Options -> String -> String
lookupParam name options def = 
   case lookupBST name options of
      Just (ParamValue v) -> v
      _                   -> def
\end{code}

\noindent
\highlighttt{lookupQueue}~$\mathit{name}~\mathit{options}$
returns the list stored for the $\mathit{name}$ed
queue parameter in $\mathit{options}$ or {\tt []} if it
has not been properly specified.
      
\begin{code}
lookupQueue :: String -> Options -> [String]
lookupQueue name options = 
   case lookupBST name options of
      Just (ParamQueue q) -> extractQ q
      _                   -> []
\end{code}

