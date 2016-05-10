\module{Priorities}

The {\tt Priority} module defines a data type for
representing the superiority relation for Defeasible
and Plausible logic.

\begin{code}
module Priority(
      Priority(( :> )), priorityP, countPriorities, cycles
   ) where
\end{code}

\begin{code}
import ABR.Data.BSTree; import ABR.Parser
import ABR.DeepSeq
\end{code}

\begin{code}
import Label
\end{code}

\begin{code}
infix 4 :>
\end{code}

\submodule{Data type definition} %%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
data Priority = !Label :> !Label
                deriving (Eq, Ord)
\end{code}

\submodule{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The syntax for a priority declaration is:

\input{\texInc prioritySyntax.tex}

\noindent which is implemented:

\begin{code}
priorityP :: Parser Priority
priorityP = labelP ABR.Parser.<*> literalP "symbol" ">"
            ABR.Parser.*> nofail' "label expected" labelP
            @> uncurry (:>)
\end{code}


\submodule{Testing for cycles} %%%%%%%%%%%%%%%%%%%%%%%%%%%

In the Defeasible and Plausible logics, cycles in
the priority relation are not permitted. The following
is sufficient to detect cycles, but can not identify
only those priorities that contribute directly to
cycles.

\begin{figure}[htbp]
   \centering
   \epsfig{figure=\figs PriorityGraphFig.pdf}
   \caption{\setMyFontSize\label{priorityGraphFig}
            A priority relation represented as
            directed graphs, before and after cycle
	    detection.}
\end{figure}

The algorithm is to count the number of times each label
is superior and inferior. Then delete any priority
where the label at either end has either count equal
to zero. Repeat until no progress is made. Then all
remaining priorities are either involved in a cycle
or involved in a connection between two cycles. For
example, figure~\ref{priorityGraphFig} shows a priority
relation before and after the application of
{\tt cycles} as directed graphs. The nodes are labels.
The edges are priorities. The unfilled node is not
involved in any cycle but is not removed.

\begin{code}
type LCount = Int  -- # times on left of :>
type RCount = Int  -- # times on right of :>
type LRCounts = BSTree Label (LCount,RCount)

countPriorities :: [Priority] -> LRCounts
countPriorities
   = let count1L :: Label -> LRCounts -> LRCounts
         count1L l
            = updateBST (\ _ (l,r) -> (l + 1, r)) l (1,0)
         count1R :: Label -> LRCounts -> LRCounts
         count1R l
            = updateBST (\ _ (l,r) -> (l, r + 1)) l (0,1)
         count1 :: Priority -> LRCounts -> LRCounts
         count1 (lL :> lR) = (count1L lL) . (count1R lR)
     in foldr count1 emptyBST

pruneAcyclicLabels :: [Priority] -> [Priority]
pruneAcyclicLabels ps
   = let counts :: LRCounts
         counts = countPriorities ps
         isCyclic :: Priority -> Bool
         isCyclic (lL :> lR)
            = let Just (nLL,nLR) = lookupBST lL counts
                  Just (nRL,nRR) = lookupBST lR counts
              in nLL /= 0 && nLR /= 0 && nRL /= 0
	         && nRR /= 0
     in filter isCyclic ps
\end{code}

{\tt cycles ps} returns the priorities in that may be
involved in cycles in {\tt ps}. The empty list is
returned iff there are no cycles in {\tt ps}.

\begin{code}
cycles :: [Priority] -> [Priority]
cycles ps = let ps' = pruneAcyclicLabels ps
            in if length ps' == length ps
                  then ps
                  else cycles ps'
\end{code}


\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Show Priority where

   showsPrec p (l :> l')
      = shows l . showString " > " . shows l'

instance HasLabelNames Priority where

   getLabelNames (l :> l')
      = getLabelNames l . getLabelNames l'
\end{code}

\subsubmodule{Forced Evaluation}

\begin{code}
instance DeepSeq Priority where
\end{code}

\begin{code}
   deepSeq (l :> l') x = deepSeq l $ deepSeq l' x
\end{code}
