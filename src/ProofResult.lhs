\module{Proof Results} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The module {\tt ProofResult} implements a data type
that represents all the possible results on attempting
a proof.

\begin{code}
module ProofResult(
      ProofResult(..), WFResult(..)
   ) where
\end{code}

\begin{code}
import ThreadedTest
\end{code}

\submodule{Data type} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An attempted proof may at a given point in time, have
been definitely proved, definitely not proved, known
to loop, or be still in progress.

\begin{code}
data ProofResult =
     Yes          -- Proved True
   | No           -- Definitely False
   | Bottom       -- Loop detected
   | Pending      -- Still waiting to find out
   | NotAttempted -- Proof never attempted
   deriving (Eq, Ord)
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

Textual output.

\begin{code}
instance Show ProofResult where
   showsPrec p Yes          = showString "Proved"
   showsPrec p No           = showString "Not proved"
   showsPrec p Bottom       = showString "Loops"
   showsPrec p Pending      = showString "Pending"
   showsPrec p NotAttempted = showString "Not Attempted"
\end{code}

Threading tests.

\begin{code}
instance ThreadedResult ProofResult where
\end{code}

\begin{code}
   mkTest b s = return (if b then Yes else No, s)
\end{code}

\begin{code}
   (&&&) t1 t2 s = do
      (r1,s1) <- t1 s
      case r1 of
         Yes          -> t2 s1
         No           -> return (r1, s1)
         Bottom       -> return (r1, s1)
         Pending      -> error "Pending in &&&"
         NotAttempted -> error "NotAttempted in &&&"
\end{code}

\begin{code}
   (|||) t1 t2 s = do
      (r1,s1) <- t1 s
      case r1 of
         Yes          -> return (r1, s1)
         No           -> t2 s1
         Bottom       -> t2 s1
         Pending      -> error "Pending in |||"
         NotAttempted -> error "NotAttempted in |||"
\end{code}

\begin{code}
   fA' ts s  = case ts of
      []         -> return (Yes, s)
      [t]        -> t s
      (t1:t2:ts) -> do
         (r1,s1) <- t1 s
         case r1 of
            Yes          -> fA' (t2:ts) s1
            No           -> return (r1,s1)
            Bottom       -> return (r1,s1)
            Pending      -> error "Pending in fA'"
            NotAttempted -> error "NotAttempted in fA'"
\end{code}

\begin{code}
   tE' ts s = case ts of
      []         -> return (No, s)
      [t]        -> t s
      (t1:t2:ts) -> do
         (r1,s1) <- t1 s
         case r1 of
            Yes          -> return (r1,s1)
            No           -> tE' (t2:ts) s1
            Bottom       -> tE' (t2:ts) s1
            Pending      -> error "Pending in tE'"
            NotAttempted -> error "NotAttempted in tE'"
\end{code}


\submodule{Well-founded variant} %%%%%%%%%%%%%%%%%%%%%%%%%

This variant proof result type allows the implementation
of well-founded provers. This makes a difference only
when loop detection is available. The result
bottom (loops) is not propagated and gets changed
to not proved.

\begin{code}
data WFResult =
     WFYes     -- Proved True
   | WFNo      -- Definitely False
   | WFBottom  -- Loop detected
   | WFPending -- Still waiting to find out
   | WFNotAtt  -- Proof never attempted
   deriving (Eq, Ord)
\end{code}

\begin{code}
instance Show WFResult where
   showsPrec p WFYes     = showString "Proved"
   showsPrec p WFNo      = showString "Not proved"
   showsPrec p WFBottom  = showString "Loops"
   showsPrec p WFPending = showString "Pending"
   showsPrec p WFNotAtt  = showString "Not Attempted"
\end{code}

\begin{code}
instance ThreadedResult WFResult where
\end{code}

\begin{code}
   mkTest b s = return (if b then WFYes else WFNo, s)
\end{code}

\begin{code}
   (&&&) t1 t2 s = do
      (r1,s1) <- t1 s
      case r1 of
         WFYes     -> do
	    (r2,s2) <- t2 s1
	    case r2 of
	       WFYes     -> return (r2, s2)
	       WFNo      -> return (r2, s2)
	       WFBottom  -> return (WFNo, s2)
               WFPending -> error "Pending in &&&"
               WFNotAtt  -> error "NotAttempted in &&&"
         WFNo      -> return (r1, s1)
         WFBottom  -> return (WFNo, s1)
         WFPending -> error "Pending in &&&"
         WFNotAtt  -> error "NotAttempted in &&&"
\end{code}

\begin{code}
   (|||) t1 t2 s = do
      (r1,s1) <- t1 s
      case r1 of
         WFYes     -> return (r1, s1)
         WFNo      -> do
	    (r2,s2) <- t2 s1
	    case r2 of
	       WFYes     -> return (r2, s2)
	       WFNo      -> return (r2, s2)
	       WFBottom  -> return (WFNo, s2)
               WFPending -> error "Pending in |||"
               WFNotAtt  -> error "NotAttempted in |||"
         WFBottom  -> do
	    (r2,s2) <- t2 s1
	    case r2 of
	       WFYes     -> return (r2, s2)
	       WFNo      -> return (r2, s2)
	       WFBottom  -> return (WFNo, s2)
               WFPending -> error "Pending in |||"
               WFNotAtt  -> error "NotAttempted in |||"
         WFPending -> error "Pending in |||"
         WFNotAtt  -> error "NotAttempted in |||"
\end{code}

\begin{code}
   fA' ts s  = case ts of
      []         -> return (WFYes, s)
      [t]        -> do
         (r1,s1) <- t s
	 case r1 of
            WFYes     -> return (r1,s1)
            WFNo      -> return (r1,s1)
            WFBottom  -> return (WFNo,s1)
            WFPending -> error "Pending in fA'"
            WFNotAtt  -> error "NotAttempted in fA'"
      (t1:t2:ts) -> do
         (r1,s1) <- t1 s
         case r1 of
            WFYes     -> fA' (t2:ts) s1
            WFNo      -> return (r1,s1)
            WFBottom  -> return (WFNo,s1)
            WFPending -> error "Pending in fA'"
            WFNotAtt  -> error "NotAttempted in fA'"
\end{code}

\begin{code}
   tE' ts s = case ts of
      []         -> return (WFNo, s)
      [t]        -> do
         (r1,s1) <- t s
	 case r1 of
            WFYes     -> return (r1,s1)
            WFNo      -> return (r1,s1)
            WFBottom  -> return (WFNo,s1)
            WFPending -> error "Pending in tE'"
            WFNotAtt  -> error "NotAttempted in tE'"
      (t1:t2:ts) -> do
         (r1,s1) <- t1 s
         case r1 of
            WFYes     -> return (r1,s1)
            WFNo      -> tE' (t2:ts) s1
            WFBottom  -> tE' (t2:ts) s1
            WFPending -> error "Pending in tE'"
            WFNotAtt  -> error "NotAttempted in tE'"
\end{code}
