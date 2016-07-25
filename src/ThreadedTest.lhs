\module{Threaded Tests} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The module {\tt ThreadedTest} implements abstractions
and combiners that allow the treading of proofs and
state through monads; for example the {\tt IO} or
{\tt ST} monads.

\begin{code}
module ThreadedTest(
      ThreadedTest,
      ThreadedResult(mkTest, (&&&), (|||),
                     fA', tE', fA, tE)
   ) where
import DebugUtils.Trace
\end{code}

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A test must be performed. We need the result (of
type {\tt r}) returned, and a state (of type {\tt s})
may be updated. There may be other side effects, so
all of this is threaded through some monad {\tt m}.

\begin{code}
type ThreadedTest m r s = s -> m (r, s)
\end{code}

\submodule{Combining threaded tests} %%%%%%%%%%%%%%%%%%%%%

{\tt mkTest b} promotes some simple Boolean result {\tt b}
to a {\tt ThreadedTest}.
\verb"&&&" and \verb"||| conjoin and disjoin two threaded
tests. {\tt fA} and {\tt tE} are $\forall$ and $\exists$
respectively.

\begin{code}
class ThreadedResult r where
\end{code}

\begin{code}
   infixr 3 &&&
   infixr 2 |||
\end{code}

\begin{code}
   mkTest :: Monad m => Bool -> ThreadedTest m r s
\end{code}

\begin{code}
   (&&&), (|||) :: Monad m => ThreadedTest m r s -> ThreadedTest m r s -> ThreadedTest m r s
\end{code}

\begin{code}
   fA', tE' :: Monad m => [ThreadedTest m r s]
                          -> ThreadedTest m r s
\end{code}

\begin{code}
   fA, tE :: Monad m => [b] -> (b -> ThreadedTest m r s)
                        -> ThreadedTest m r s
   fA xs p = trace("In ThreadedTest.lhs on line 59 fA") fA' (map p xs)
   tE xs p = trace("In ThreadedTest.lhs on line 59 tE")tE' (map p xs)
\end{code}
