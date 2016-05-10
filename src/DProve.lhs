\module{DProve}

This module implements provers for Defeasible logic.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
\end{code}

\begin{code}
module DProve(
      prove, Hist, WFHist
   ) where
\end{code}

\begin{code}
import System.CPUTime
\end{code}

\begin{code}
import ABR.Args; import ABR.Data.BSTree
\end{code}

\begin{code}
import Literal; import DRule; import Priority
import ThreadedTest; import ProofResult
import History; import DTheory; import DInference
\end{code}

\submodule{Defeasible logic instance} %%%%%%%%%%%%%%%%%%%

This instance implements the functions required by the
inference conditions to use the simple theory type.

\begin{code}
instance DefeasibleLogic DTheory LabeledRule Literal where
\end{code}

\begin{code}
   isFactIn q (Theory fs _ _) = mkTest (q `elem` fs)
\end{code}

\begin{code}
   notFactIn q (Theory fs _ _) = mkTest (q `notElem` fs)
\end{code}

\begin{code}
   rq (Theory _ rs _) q
      = filter (\r -> consequent r == q) rs
\end{code}

\begin{code}
   rsq (Theory _ rs _) q
      = filter (\r -> isStrict r && consequent r == q) rs
\end{code}

\begin{code}
   rsdq (Theory _ rs _) q
      = filter (\r -> (isStrict r || isPlausible r)
                && consequent r == q) rs
\end{code}

\begin{code}
   ants t r = antecedent r
\end{code}

\begin{code}
   beats (Theory _ _ ps) (Rule l _) (Rule l' _)
      = mkTest ((l :> l') `elem` ps)
\end{code}

\begin{code}
   notBeats (Theory _ _ ps) (Rule l _) (Rule l' _)
      = mkTest ((l :> l') `notElem` ps)
\end{code}


\submodule{Provers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\verb"prove_ t tl ()" returns \verb"(r,())", where
{\tt r} is the result of trying to prove tagged literal
{\tt tl} with theory {\tt t}. This is the simplest prover,
with no trace, no history and therefore no loop checking,
and not well founded.

\begin{code}
prove_ :: Theory -> Tagged Literal
	  -> ThreadedTest Maybe ProofResult ()
prove_ t tl () = (t |-- tl) prove_ ()
\end{code}


\verb"prove_n t tl 0" returns \verb"(r,ng)", where
{\tt r} is the result of trying to prove tagged literal
{\tt tl} with theory {\tt t} and {\tt ng} is the number
of subgoals required to do so.

\begin{code}
prove_n :: Theory -> Tagged Literal
	   -> ThreadedTest Maybe ProofResult Int
prove_n t tl ng = do
   (r, ng') <- (t |-- tl) prove_n ng
   return (r, ng' + 1)
\end{code}


\verb'prove_t t tl ""' returns \verb'(r,"")',
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}. A trace is printed.

\begin{code}
prove_t :: Theory -> Tagged Literal
	   -> ThreadedTest IO ProofResult String
prove_t t tl indent = do
   putStrLn (indent ++ "To Prove: " ++ show tl)
   (r, _) <-
      (t |-- tl) prove_t (".  " ++ indent)
   putStrLn (indent ++ show r ++ ": " ++ show tl)
   return (r, indent)
\end{code}


\verb'prove_nt t tl (0,"")' returns \verb'(r,(ng,""))',
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t} and {\tt ng} is the
number of subgoals required to do so. A trace is printed.

\begin{code}
prove_nt :: Theory -> Tagged Literal
	    -> ThreadedTest IO ProofResult (Int,String)
prove_nt t tl (ng,indent) = do
   putStrLn (indent ++ "To Prove: " ++ show tl)
   (r, (ng',_)) <-
      (t |-- tl) prove_nt (ng, ".  " ++ indent)
   putStrLn (indent ++ show r ++ ": " ++ show tl)
   return (r, (ng' + 1, indent))
\end{code}


This type is shorthand for the history that maps
tagged literals to prior results.

\begin{code}
type Hist = History (Tagged Literal) ProofResult
\end{code}


\verb"prove_nh t tl (0,h)" returns \verb"(r,(ng,h'))",
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}, {\tt ng} is the
number of subgoals required to do so, {\tt h} is
a history of prior results and \verb"h'" is the final
history. This prover avoids redoing prior proofs, but
does not perform loop checking.

\begin{code}
prove_nh :: Theory -> Tagged Literal
	    -> ThreadedTest Maybe ProofResult (Int, Hist)
prove_nh t tl (ng,h) = case getResult h tl of
   Just r ->
      return (r, (ng,h))
   Nothing -> do
      (r, (ng',h')) <- (t |-- tl) prove_nh (ng,h)
      return (r, (ng' + 1, addProof h' tl r))
\end{code}


\verb+prove_nht t tl (0,h,"")+ returns
\verb+(r,(ng,h',""))+, where {\tt r} is the result of
trying to prove tagged literal {\tt tl} with theory
{\tt t}, {\tt ng} is the number of subgoals required
to do so, {\tt h} is a history of prior results and
\verb"h'" is the final history. This prover avoids
redoing prior proofs, but does not perform loop
checking. A trace is printed.

\begin{code}
prove_nht
   :: Theory -> Tagged Literal
      -> ThreadedTest IO ProofResult (Int,Hist,String)
prove_nht t tl (ng,h,indent) = case getResult h tl of
   Just r -> do
      putStrLn (indent ++ show r ++ " previously: "
                ++ show tl)
      return (r, (ng,h,indent))
   Nothing -> do
      putStrLn (indent ++ "To Prove: " ++ show tl)
      (r, (ng',h',_)) <-
         (t |-- tl) prove_nht (ng, h, ".  " ++ indent)
      putStrLn (indent ++ show r ++ ": " ++ show tl)
      return (r, (ng' + 1, addProof h' tl r, indent))
\end{code}


\verb"prove_nhl t tl (0,h)" returns \verb"(r,(ng,h'))",
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}, {\tt ng} is the
number of subgoals required to do so, {\tt h} is
a history of prior results and \verb"h'" is the final
history. This prover avoids redoing prior proofs, and
performs loop checking.

\begin{code}
prove_nhl :: Theory -> Tagged Literal
	    -> ThreadedTest Maybe ProofResult (Int, Hist)
prove_nhl t tl (ng,h) = case getResult h tl of
   Just Pending ->
      return (Bottom, (ng, addProof h tl Bottom))
   Just r ->
      return (r, (ng, h))
   Nothing -> do
      (r, (ng',h')) <-
          (t |-- tl) prove_nhl (ng, addProof h tl Pending)
      return (r, (ng' + 1, addProof h' tl r))
\end{code}


\verb+prove_nhlt t tl (0,h,"")+ returns
\verb+(r,(ng,h',""))+,
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}, {\tt ng} is the
number of subgoals required to do so, {\tt h} is
a history of prior results and \verb"h'" is the final
history. This prover avoids redoing prior proofs, and
performs loop checking. A trace is printed.

\begin{code}
prove_nhlt
   :: Theory -> Tagged Literal
      -> ThreadedTest IO ProofResult (Int, Hist, String)
prove_nhlt t tl (ng,h,indent) = case getResult h tl of
   Just Pending -> do
      putStrLn (indent ++ "Loop detected: " ++ show tl)
      return (Bottom, (ng, addProof h tl Bottom, indent))
   Just r -> do
      putStrLn (indent ++ show r ++ " previously: "
                ++ show tl)
      return (r, (ng, h, indent))
   Nothing -> do
      putStrLn (indent ++ "To Prove: " ++ show tl)
      (r, (ng',h',_)) <-
         (t |-- tl) prove_nhlt
	    (ng, addProof h tl Pending, ".  " ++ indent)
      putStrLn (indent ++ show r ++ ": " ++ show tl)
      let h'' = case r of
             Bottom -> h
             _      -> addProof h' tl r
      return (r, (ng' + 1, h'', indent))
\end{code}


\submodule{Provers with well-founded semantics} %%%%%%%%%%%%%

This type is shorthand for the history that maps
tagged literals to prior well-founded results.

\begin{code}
type WFHist = History (Tagged Literal) WFResult
\end{code}

\verb"prove_nhlw t tl (0,h)" returns \verb"(r,(ng,h'))",
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}, {\tt ng} is the
number of subgoals required to do so, {\tt h} is
a history of prior results and \verb"h'" is the final
history. This prover avoids redoing prior proofs,
performs loop checking, and has well-founded semantics.

\begin{code}
prove_nhlw :: Theory -> Tagged Literal
   -> ThreadedTest Maybe WFResult (Int, WFHist)
prove_nhlw t tl (ng,h) = case getResult h tl of
   Just WFPending ->
      return (WFBottom, (ng, addProof h tl WFBottom))
   Just r ->
      return (r, (ng, h))
   Nothing -> do
      (r, (ng',h')) <-
          (t |-- tl) prove_nhlw (ng, addProof h tl WFPending)
      return (r, (ng' + 1, addProof h' tl r))
\end{code}


\verb+prove_nhlwt t tl (0,h,"")+ returns
\verb+(r,(ng,h',""))+,
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}, {\tt ng} is the
number of subgoals required to do so, {\tt h} is
a history of prior results and \verb"h'" is the final
history. This prover avoids redoing prior proofs,
performs loop checking, and has well-founded semantics.
A trace is printed.

\begin{code}
prove_nhlwt :: Theory -> Tagged Literal
   -> ThreadedTest IO WFResult (Int, WFHist, String)
prove_nhlwt t tl (ng,h,indent) = case getResult h tl of
   Just WFPending -> do
      putStrLn (indent ++ "Loop detected: " ++ show tl)
      return (WFBottom, (ng, addProof h tl WFBottom, indent))
   Just r -> do
      putStrLn (indent ++ show r ++ " previously: "
                ++ show tl)
      return (r, (ng, h, indent))
   Nothing -> do
      putStrLn (indent ++ "To Prove: " ++ show tl)
      (r, (ng',h',_)) <-
         (t |-- tl) prove_nhlwt
	    (ng, addProof h tl WFPending, ".  " ++ indent)
      putStrLn (indent ++ show r ++ ": " ++ show tl)
      return (r, (ng' + 1, addProof h' tl r, indent))
\end{code}


\submodule{Prover selector} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\verb"prove t options def tl h" uses the prover engine selected
by the {\tt e} option in {\tt options}, or the default
indicated by {\tt def} if the {\tt e} option is not
present, to prove {\tt tl} using {\tt t}. {\tt h} is
a history of prior results. Updated histories and the
proof result as a string are returned.

\begin{code}
prove :: Theory -> Options -> String -> Tagged Literal
         -> Hist -> WFHist -> IO (Hist, WFHist, String)
prove t options def tl h wh = case lookupBST "e" options of
      Nothing -> prove t (updateBST (\x _ -> x) "e"
                    (ParamValue def) options) def tl h wh
      Just (ParamValue cs) -> case cs of
         "-"     -> use_prove_ tl
         "n"     -> use_prove_n tl
         "t"     -> use_prove_t tl
         "nt"    -> use_prove_nt tl
         "nh"    -> use_prove_nh tl
         "nht"   -> use_prove_nht tl
         "nhl"   -> use_prove_nhl tl
         "nhlt"  -> use_prove_nhlt tl
         "nhlw"  -> use_prove_nhlw tl
         "nhlwt" -> use_prove_nhlwt tl
         _      -> do
	    putStrLn $ "Error: No such prover as \""
	               ++ cs ++ "\""
	    return (h, wh, "")
   where
   use_prove_ tl = do
      time0 <- getCPUTime
      let Just (result,_) = prove_ t tl ()
      putStrLn $ show result ++ "."
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, wh, show result)
   use_prove_n tl = do
      time0 <- getCPUTime
      let Just (result,ng) = prove_n t tl 0
      putStrLn $ show result ++ "."
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, wh, show result)
   use_prove_t tl = do
      time0 <- getCPUTime
      (result,_) <- prove_t t tl ""
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, wh, show result)
   use_prove_nt tl = do
      time0 <- getCPUTime
      (result,(ng,_)) <- prove_nt t tl (0, "")
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, wh, show result)
   use_prove_nh tl = do
      time0 <- getCPUTime
      let Just (result,(ng,h')) = prove_nh t tl (0,h)
      putStrLn $ show result ++ "."
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h', wh, show result)
   use_prove_nht tl = do
      time0 <- getCPUTime
      (result,(ng,h',_)) <- prove_nht t tl (0,h,"")
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h', wh, show result)
   use_prove_nhl tl = do
      time0 <- getCPUTime
      let Just (result,(ng,h')) = prove_nhl t tl (0,h)
      putStrLn $ show result ++ "."
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h', wh, show result)
   use_prove_nhlt tl = do
      time0 <- getCPUTime
      (result,(ng,h',_)) <- prove_nhlt t tl (0,h,"")
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h', wh, show result)
   use_prove_nhlw tl = do
      time0 <- getCPUTime
      let Just (result,(ng,wh')) = prove_nhlw t tl (0,wh)
          result' = case result of
	     WFBottom -> WFNo
	     _        -> result
      putStrLn $ show result' ++ "."
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, wh', show result)
   use_prove_nhlwt tl = do
      time0 <- getCPUTime
      (result,(ng,wh',_)) <- prove_nhlwt t tl (0,wh,"")
      let result' = case result of
	     WFBottom -> WFNo
	     _        -> result
      putStrLn $ show result' ++ "."
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, wh', show result)
\end{code}
