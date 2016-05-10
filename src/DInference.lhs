\module{Inference Conditions} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module {\tt DInference} defines the inference conditions
for Defeasible logic.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
\end{code}

\begin{code}
module DInference(
      ProofSymbol(..), Tagged(..), taggedLiteralP,
      DefeasibleLogic(..)
   ) where
\end{code}

\begin{code}
import Data.Ix
\end{code}

\begin{code}
import ABR.Parser
\end{code}

\begin{code}
import Literal; import ThreadedTest
\end{code}

\submodule{Data type definitions} %%%%%%%%%%%%%%%%%%%%%%%%

A tagged literal consists of a literal, a symbol
to indicate the level of proof required, and a $+$
or $-$ sign to indicate that a proof or proof that
it can not be proved is required.
The proof symbols are defined by
table~\ref{proofSymbolTable2}.

\begin{code}
data ProofSymbol
   =   PS_D | PS_d | PS_da | PS_S | PS_dt
     deriving (Eq, Ord, Ix)
\end{code}

\begin{code}
-- (Eq a, Show a, Ord a) =>
data Tagged a
   =   Plus  !ProofSymbol !a
     | Minus !ProofSymbol !a
     deriving (Eq, Ord)
\end{code}

\begin{table}[h]

\centering

\setMyFontSize

\renewcommand{\arraystretch}[0]{1.5}

\begin{tabular}{|c|c|p{16.5em}|} \hline
\emph{constructor} & \emph{symbol} & \emph{meaning} \\ \hline
\verb"PS_D" & $\Delta$ & strict \\
\verb"PS_d" & $\partial$ & defeasible \\ \hline

\verb"PS_dt" & $\partial_{-t}$ & defeasible variant without team defeat \\
\verb"PS_da" & $\delta$ & defeasible variant with ambiguity
                          propagation \\
\verb"PS_S" & $\int$ & defeasible variant: support \\ \hline
\end{tabular}

\caption{\setMyFontSize\label{proofSymbolTable2} The
proof symbols and their Haskell representation and
meanings.}

\end{table}


\submodule{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The syntax for a tagged literal is:

\input{\texInc DTagLitSyntax.tex}

\noindent which is implemented:

\begin{code}
proofSymbolP :: Parser ProofSymbol
proofSymbolP
   =     literalP "name2" "D"    #> PS_D
     <|> literalP "name1" "d"    #> PS_d
     <|> literalP "name1" "da"   #> PS_da
     <|> literalP "name2" "S"    #> PS_S
     <|> literalP "name1" "dt"   #> PS_dt
\end{code}

\begin{code}
taggedLiteralP :: Parser (Tagged Literal)
taggedLiteralP
   = (literalP "symbol" "+" <|> literalP "symbol" "-")
     ABR.Parser.<*> nofail' "proof symbol expected" proofSymbolP
     ABR.Parser.<*> nofail' "literal expected" pLiteralP
     @> (\((_,c,_),(ps,l)) -> case c of
           "+" -> Plus ps l
	   "-" -> Minus ps l)
\end{code}


\submodule{Overloaded functions} %%%%%%%%%%%%%%%%%%%%%%%%%

Class {\tt DefeasibleLogic} overloads the some functions
that the inference conditions are defined in terms of
to hide (and generalize) the representation of theories,
labels and rules. Then the inference conditions need only
be specified once. This class has multiple type parameters,
and therefore relies on Hugs and GHC extensions. The
parameters {\tt th}, {\tt rul}, and {\tt lit} are the
names of the theory, rule, and literal types. The type
for rules must be parameterized by the type for literals,
and the type for theories must be parameterized by the type
for rules.

\begin{code}
class (Negatable lit, Show lit, Eq lit, Ord lit) =>
   DefeasibleLogic th rul lit where
\end{code}

\begin{code}
   infix 6 |--
\end{code}

The following methods need to be defined for instances
of this class.

{\tt isFactIn~q~t} is a test whether {\tt q} is a fact
in theory {\tt t}. {\tt notFactIn~q~t} returns the
opposite result.

\begin{code}
   isFactIn, notFactIn :: (Monad m, ThreadedResult r) =>
      lit -> th (rul lit)  -> ThreadedTest m r s
\end{code}

{\tt rq~t~q} returns the list of rules in {\tt t} that
have consequent {\tt q}. {\tt rsq~t~q} returns the list
of strict rules in {\tt t} that have consequent {\tt q}.
{\tt rsdq~t~q} returns the list of rules in {\tt t} that
have consequent {\tt q} and are strict or defeasible.

\begin{code}
   rq, rsq, rsdq :: th (rul lit)  -> lit -> [rul lit]
\end{code}

{\tt ants~t~r} returns the list of literals that are
the antecedents of rule {\tt r} in theory {\tt t}.

\begin{code}
   ants :: th (rul lit)  -> rul lit -> [lit]
\end{code}

{\tt beats~t~r1~r2} is a test whether there exists in
{\tt t} a priority that asserts that {\tt r1} is
superior to {\tt r2}. {\tt notBeats~t~r1~r2} returns
the opposite result.

\begin{code}
   beats, notBeats :: (Monad m, ThreadedResult r) =>
      th (rul lit) -> rul lit -> rul lit
      -> ThreadedTest m r s
\end{code}


\submodule{Inference Conditions} %%%%%%%%%%%%%%%%%%%%%%%%%

\verb"t |-- tl (|-)" is a test whether the tagged literal
{\tt tl} can be proved from theory {\tt t}. The definition of
this function is shown in figure~\ref{infConditsFig} along
with the inference conditions it implements.
\verb"|-" is the main proof function that is mutually
recursive with this one. \verb"|-" handles all state
manipulations and/or I/O.

\begin{code}
   (|--) :: (Monad m, ThreadedResult r) =>
      th (rul lit) -> Tagged lit -> (th (rul lit) ->
      Tagged lit -> ThreadedTest m r s)
      -> ThreadedTest m r s
\end{code}

\begin{figure*}[!t]

\setMyFontSize

\centering

\begin{tabular}{cp{42em}}

\raisebox{-1.25em}{$+\Delta$:} &

\begin{code}
   (|--) t (Plus PS_D q) (|-)
      = q `isFactIn` t |||
        tE (rsq t q) (\r -> fA (ants t r) (\a -> t |- Plus PS_D a))
\end{code}

\\

\raisebox{-1.25em}{$-\Delta$:} &

\begin{code}
   (|--) t (Minus PS_D q) (|-)
      = q `notFactIn` t &&&
        fA (rsq t q) (\r -> tE (ants t r) (\a -> t |- Minus PS_D a))
\end{code}

\\

\raisebox{-1.25em}{$+\partial$:} &

\begin{code}
   (|--) t (Plus PS_d q) (|-)
      = t |- Plus PS_D q |||
           tE (rsdq t q) (\r -> fA (ants t r) (\a -> t |- Plus PS_d a)) &&&
           t |- Minus PS_D (neg q) &&&
           fA (rq t (neg q)) (\s ->
	      tE (ants t s) (\a -> t |- Minus PS_d a) |||
              tE (rsdq t q) (\u ->
	         fA (ants t u) (\a -> t |- Plus PS_d a) &&& beats t u s))
\end{code}

\\

\raisebox{-1.25em}{$-\partial$:} &

\begin{code}
   (|--) t (Minus PS_d q) (|-)
      = t |- Minus PS_D q &&& (
           fA (rsdq t q) (\r -> tE (ants t r) (\a -> t |- Minus PS_d a)) |||
           t |- Plus PS_D (neg q) |||
           tE (rq t (neg q)) (\s ->
	      fA (ants t s) (\a -> t |- Plus PS_d a) &&&
              fA (rsdq t q) (\u ->
	         tE (ants t u) (\a -> t |- Minus PS_d a) ||| notBeats t u s)))
\end{code}

\end{tabular}

\caption{\setMyFontSize\label{infConditsFig}Inference conditions
for defeasible logic.}

\end{figure*}

Additional inference conditions for variants of
Defeasible logic that feature ambiguity propagation ($\pm \delta$ and $\pm \int$)
and variants that do not feature team defeat ($\pm\partial_{-t}$)
have also been implemented and
are shown in figure~\ref{infConditsFig2}.

\begin{figure*}[!t]

\setMyFontSize

\centering

\begin{tabular}{cp{42em}}

\raisebox{-1.25em}{$+\delta$:} &

\begin{code}
   (|--) t (Plus PS_da q)  (|-)
      = t |- Plus PS_D q |||
           tE (rsdq t q) (\r -> fA (ants t r) (\a -> t |- Plus PS_da a)) &&&
           t |- Minus PS_D (neg q) &&&
           fA (rq t (neg q)) (\s ->
	      tE (ants t s) (\a -> t |- Minus PS_S a) |||
              tE (rsdq t q) (\u ->
	         fA (ants t u) (\a -> t |- Plus PS_da a) &&& beats t u s))
\end{code}

\\

\raisebox{-1.25em}{$-\delta$:} &

\begin{code}
   (|--) t (Minus PS_da q)  (|-)
      = t |- Minus PS_D q &&& (
           fA (rsdq t q) (\r -> tE (ants t r) (\a -> t |- Minus PS_da a)) |||
           t |- Plus PS_D (neg q) |||
           tE (rq t (neg q)) (\s ->
	      fA (ants t s) (\a -> t |- Plus PS_S a) &&&
              fA (rsdq t q) (\u ->
	         tE (ants t u) (\a -> t |- Minus PS_da a) ||| notBeats t u s)))
\end{code}

\\

\raisebox{-1.25em}{$+\int $:} &

\begin{code}
   (|--) t (Plus PS_S q)  (|-)
      = t |- Plus PS_D q |||
           tE (rsdq t q) (\r ->
	      fA (ants t r) (\a -> t |- Plus PS_S a) &&&
	      fA (rq t (neg q)) (\s ->
	         tE (ants t s) (\a -> t |- Minus PS_da a) ||| notBeats t s r))
\end{code}

\\

\raisebox{-1.25em}{$-\int $:} &

\begin{code}
   (|--) t (Minus PS_S q)  (|-)
      = t |- Minus PS_D q &&&
           fA (rsdq t q) (\r ->
	      tE (ants t r) (\a -> t |- Minus PS_S a) |||
	      tE (rq t (neg q)) (\s ->
	         fA (ants t s) (\a -> t |- Plus PS_da a) &&& beats t s r))
\end{code}

\\


\raisebox{-1.25em}{$+\partial_{-t}$:} &

\begin{code}
   (|--) t (Plus PS_dt q) (|-)
      = t |- Plus PS_D q |||
           tE (rsdq t q) (\r -> fA (ants t r) (\a -> t |- Plus PS_dt a) &&&
	   t |- Minus PS_D (neg q) &&&
	   fA (rq t (neg q)) (\s ->
	      beats t r s |||
	      tE (ants t s) (\a -> t |- Minus PS_dt a)))
\end{code}

\\

\raisebox{-1.25em}{$-\partial_{-t}$:} &

\begin{code}
   (|--) t (Minus PS_dt q) (|-)
      = t |- Minus PS_D q &&& (
           fA (rsdq t q) (\r -> tE (ants t r) (\a -> t |- Minus PS_dt a) |||
	   t |- Plus PS_D (neg q) |||
	   tE (rq t (neg q)) (\s ->
	      notBeats t r s &&&
	      fA (ants t s) (\a -> t |- Plus PS_dt a))))
\end{code}

\\

\end{tabular}

\caption{\setMyFontSize\label{infConditsFig2}Inference conditions
for variants of defeasible logic.}

\end{figure*}


\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

Textual output.

\begin{code}
instance Show ProofSymbol where
\end{code}

\begin{code}
   showsPrec p ps
      = case ps of
           PS_D   -> showChar   'D'
           PS_d   -> showChar   'd'
           PS_da  -> showString "da"
           PS_S   -> showChar   'S'
           PS_dt  -> showString "dt"
\end{code}

\begin{code}
instance (Show a, Eq a, Ord a) => Show (Tagged a) where
\end{code}

\begin{code}
   showsPrec p t = case t of
      Plus ps q  -> showChar '+' . shows ps . showChar ' '
                    . shows q
      Minus ps q -> showChar '-' . shows ps . showChar ' '
                    . shows q
\end{code}


Extracting literal names

\begin{code}
instance (HasLits a, Show a, Eq a, Ord a) =>
   HasLits (Tagged a) where
\end{code}

\begin{code}
   getLits t s = case t of
      Plus  _ q -> getLits q s
      Minus _ q -> getLits q s
\end{code}

Detecting variable names.

\begin{code}
instance (HasVarNames a, Show a, Ord a) =>
   HasVarNames (Tagged a) where
\end{code}

\begin{code}
   getVarNames tl s = case tl of
      Plus  _ l -> getVarNames l s
      Minus _ l -> getVarNames l s
\end{code}
