\module{Optimized Theories} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module {\tt ODTheory} defines a data type for storage
of Defeasible logic theories that facilitates faster
proofs.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances,FlexibleInstances  #-}
\end{code}

\begin{code}
module ODTheory(
      ORule, OPriorities, OTheory, makeOTheory,
      makeOTL, unmakeOTL, showOTL, OHist, oprove,
      FHist, initPmSyLitHist
   ) where
\end{code}

\begin{code}
import Control.Monad.ST; import System.CPUTime
import Data.Array.ST; import Data.Array
\end{code}

\begin{code}
import ABR.Data.BSTree; import ABR.SparseSet; import ABR.Args
import ABR.Graph
\end{code}

\begin{code}
import Literal; import DRule; import Label
import Priority; import ThreadedTest
import DInference; import DTheory
import ProofResult; import History
\end{code}

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

All the facts should be stored in an array that maps
each literal to {\tt True} (it's a fact) or {\tt False}
(it's not).

\begin{code}
type OFacts = Array OLiteral Bool
\end{code}

All the rules will be stored in parallel arrays of the
antecedents and consequents. An {\tt ORule} is the index
type for these arrays.

\begin{code}
data ORuleIndex lit = OR Int
                      deriving (Eq, Ord, Show)
\end{code}

\begin{code}
type ORule = ORuleIndex OLiteral
\end{code}

\begin{code}
type OAnts = Array ORule [OLiteral]
\end{code}

\begin{code}
type OCons = Array ORule OLiteral
\end{code}

We can presort the rules by consequent.

\begin{code}
type ORuleTable = Array OLiteral [ORule]
\end{code}

The priorities are a graph.

\begin{code}
type OPriorities = SGraph ORule
\end{code}

A complete theory ready to use:

\begin{code}
data ODTheory rul = OTheory {
      num2name   :: LitArray,
      name2num   :: LitTree,
      facts      :: OFacts,
      cons       :: OCons,
      antes      :: OAnts,
      plausStart :: ORule,
      defStart   :: ORule,
      priorities :: OPriorities,
      prsq       :: ORuleTable,
      prsdq      :: ORuleTable,
      prq        :: ORuleTable
   }
\end{code}

\begin{code}
type OTheory = ODTheory ORule
\end{code}

\noindent {\tt plausStart} is the index in the rules
arrays where the rules turn from strict to plausible,
and {\tt defStart} is the index at which rules start
being defeaters.


\submodule{Building an optimized theory} %%%%%%%%%%%%%%%%%

{\tt makeOTheory s t} builds an optimized theory
using the set of literal names {\tt s} and
theory {\tt t}.

\begin{code}
makeOTheory :: SparseSet Literal -> Theory -> OTheory
makeOTheory s t@(Theory fs rs ps) = let
      (num2nam,nam2num) = makeLitTables s
      (_,nLit) = bounds num2nam
      srs = filter isStrict rs
      prs = filter isPlausible rs
      drs = filter isDefeater rs
      rs' = srs ++ prs ++ drs
      n_srs = length srs
      n_prs = length prs
      n_drs = length drs
      n_rs = n_srs + n_prs + n_drs
      cons' =
         listArray (OR 0, OR (n_rs - 1))
         (map (toOLiteral nam2num . consequent) rs')
      labelTable =
         pairs2BST $ filter (not . null . fst)
         $ zip (map (\(Rule (Label l) _) -> l) rs') [0..]
      toRuleIndex :: Label -> ORule
      toRuleIndex (Label l) =
         case lookupBST l labelTable of
            Just i  -> i
            Nothing -> error "toRuleIndex: Label not found"
      crs = map (\(x,y) -> (y,x)) $ assocs cons'
   in OTheory {
      num2name = num2nam,
      name2num = nam2num,
      facts =
         accumArray (\ _ _ -> True) False (-nLit, nLit)
         $ map (\l -> (toOLiteral nam2num l, True)) fs,
      cons = cons',
      antes =
         listArray (OR 0, OR n_rs - 1) (map ((map
         (toOLiteral nam2num)) . antecedent) rs'),
      plausStart = OR n_srs,
      defStart = OR (n_srs + n_prs),
      priorities =
         mkGraph (OR 0) (OR n_rs - 1)
	 (map (\(l1 :> l2) ->
	    (toRuleIndex l1, toRuleIndex l2))
          ps),
      prsq =
         accumArray (flip (:)) [] (- nLit, nLit)
         $ take n_srs crs,
      prsdq =
         accumArray (flip (:)) [] (- nLit, nLit)
         $ take (n_srs + n_prs) crs,
      prq = accumArray (flip (:)) [] (- nLit, nLit) crs
   }
\end{code}


\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Num (ORuleIndex lit) where
\end{code}

\begin{code}
   OR a + OR b = OR (a + b)
\end{code}

\begin{code}
   OR a - OR b = OR (a - b)
\end{code}

\begin{code}
   OR a * OR b = OR (a * b)
\end{code}

\begin{code}
   abs (OR a) = OR (abs a)
\end{code}

\begin{code}
   signum (OR a) = OR (signum a)
\end{code}

\begin{code}
   fromInteger = OR . fromInteger
\end{code}

\begin{code}
instance Enum (ORuleIndex lit) where
\end{code}

\begin{code}
   toEnum = OR
\end{code}

\begin{code}
   fromEnum (OR i) = i
\end{code}

\begin{code}
   enumFrom (OR i) = map OR [i..]
\end{code}

\begin{code}
instance Ix (ORuleIndex lit) where
\end{code}

\begin{code}
   range (a,b) = [a..b]
\end{code}

\begin{code}
   index (OR i,_) (OR j) = j - i
\end{code}

\begin{code}
   inRange (OR i,OR j) (OR k) = i <= k && k <= j
\end{code}

\begin{code}
instance Show OTheory where
\end{code}

\begin{code}
   showsPrec p t =
      showString "num2name: "       . shows (num2name t)
      . showString "\nname2num: "   . shows (name2num t)
      . showString "\nfacts: "      . shows (facts t)
      . showString "\ncons: "       . shows (cons t)
      . showString "\nants: "       . shows (antes t)
      . showString "\nplausStart: " . shows (plausStart t)
      . showString "\ndefStart: "   . shows (defStart t)
      . showString "\npriorities: " . shows (priorities t)
      . showString "\nprsq: "       . shows (prsq t)
      . showString "\nprsdq: "      . shows (prsdq t)
      . showString "\nprq: "        . shows (prq t)
\end{code}

\begin{code}
instance DefeasibleLogic ODTheory ORuleIndex OLiteral where
\end{code}

\begin{code}
   isFactIn q t = mkTest (facts t ! q)
\end{code}

\begin{code}
   notFactIn q t = mkTest (not (facts t ! q))
\end{code}

\begin{code}
   rq t q = prq t ! q
\end{code}

\begin{code}
   rsq t q = prsq t ! q
\end{code}

\begin{code}
   rsdq t q = prsdq t ! q
\end{code}

\begin{code}
   ants t r = antes t ! r
\end{code}

\begin{code}
   beats t r1 r2 = mkTest $ isAdjacent (priorities t) r1 r2
\end{code}

\begin{code}
   notBeats t r1 r2
      = mkTest $ not $ isAdjacent (priorities t) r1 r2
\end{code}


\submodule{Optimized tagged literals} %%%%%%%%%%%%%%%%%%%%

{\tt makeOTL ot tl} converts tagged literal  {\tt tl}
to an optimized tagged literal using the mapping
to optimized literals in optimized theory {\tt ot}.
{\tt unmakeOTL} performs the reverse operation.
{\tt showOTL} uses {\tt unmakeOTL} before showing
an optimized literal so that the true name is
shown, rather than the number.

\begin{code}
makeOTL :: OTheory -> Tagged Literal -> Tagged OLiteral
makeOTL ot tl = case tl of
   Plus  ps l -> Plus  ps (toOLiteral (name2num ot) l)
   Minus ps l -> Minus ps (toOLiteral (name2num ot) l)
\end{code}

\begin{code}
unmakeOTL :: OTheory -> Tagged OLiteral -> Tagged Literal
unmakeOTL ot otl = case otl of
   Plus  ps ol -> Plus  ps (fromOLiteral (num2name ot) ol)
   Minus ps ol -> Minus ps (fromOLiteral (num2name ot) ol)
\end{code}

\begin{code}
showOTL :: OTheory -> Tagged OLiteral -> String
showOTL ot = show . unmakeOTL ot
\end{code}


\submodule{Provers without histories} %%%%%%%%%%%%%%%%%%%%

\verb"oprove_ t tl ()" returns \verb"(r,())", where
{\tt r} is the result of trying to {\tt oprove} tagged literal
{\tt tl} with theory {\tt t}. This is the simplest prover,
with no trace, no history and therefore no loop checking,
and not well founded.

\begin{code}
oprove_ :: OTheory -> Tagged OLiteral
	  -> ThreadedTest Maybe ProofResult ()
oprove_ t tl () = (t |-- tl) oprove_ ()
\end{code}


\verb"oprove_n t tl 0" returns \verb"(r,ng)", where
{\tt r} is the result of trying to {\tt oprove} tagged literal
{\tt tl} with theory {\tt t} and {\tt ng} is the number
of subgoals required to do so.

\begin{code}
oprove_n :: OTheory -> Tagged OLiteral
	   -> ThreadedTest Maybe ProofResult Int
oprove_n t tl ng = do
   (r, ng') <- (t |-- tl) oprove_n ng
   return (r, ng' + 1)
\end{code}


\verb'oprove_t t tl ""' returns \verb'(r,"")',
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}. A trace is printed.

\begin{code}
oprove_t :: OTheory -> Tagged OLiteral
	   -> ThreadedTest IO ProofResult String
oprove_t t tl indent = do
   putStrLn (indent ++ "To Prove: " ++ showOTL t tl)
   (r, _) <-
      (t |-- tl) oprove_t (".  " ++ indent)
   putStrLn (indent ++ show r ++ ": " ++ showOTL t tl)
   return (r, indent)
\end{code}


\verb'oprove_nt t tl (0,"")' returns \verb'(r,(ng,""))',
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t} and {\tt ng} is the
number of subgoals required to do so. A trace is printed.

\begin{code}
oprove_nt :: OTheory -> Tagged OLiteral
	    -> ThreadedTest IO ProofResult (Int,String)
oprove_nt t tl (ng,indent) = do
   putStrLn (indent ++ "To Prove: " ++ showOTL t tl)
   (r, (ng',_)) <-
      (t |-- tl) oprove_nt (ng, ".  " ++ indent)
   putStrLn (indent ++ show r ++ ": " ++ showOTL t tl)
   return (r, (ng' + 1, indent))
\end{code}


\submodule{Provers with tree histories} %%%%%%%%%%%%%%%%%%

This type is shorthand for the history that maps
tagged literals to prior results.

\begin{code}
type OHist = History (Tagged OLiteral) ProofResult
\end{code}


\verb"oprove_nh t tl (0,h)" returns \verb"(r,(ng,h'))",
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}, {\tt ng} is the
number of subgoals required to do so, {\tt h} is
a history of prior results and \verb"h'" is the final
history. This prover avoids redoing prior proofs, but
does not perform loop checking.

\begin{code}
oprove_nh :: OTheory -> Tagged OLiteral
	    -> ThreadedTest Maybe ProofResult (Int, OHist)
oprove_nh t tl (ng,h) = case getResult h tl of
   Just r ->
      return (r, (ng,h))
   Nothing -> do
      (r, (ng',h')) <- (t |-- tl) oprove_nh (ng,h)
      return (r, (ng' + 1, addProof h' tl r))
\end{code}


\verb+oprove_nht t tl (0,h,"")+ returns
\verb+(r,(ng,h',""))+, where {\tt r} is the result of
trying to prove tagged literal {\tt tl} with theory
{\tt t}, {\tt ng} is the number of subgoals required
to do so, {\tt h} is a history of prior results and
\verb"h'" is the final history. This prover avoids
redoing prior proofs, but does not perform loop
checking. A trace is printed.

\begin{code}
oprove_nht
   :: OTheory -> Tagged OLiteral
      -> ThreadedTest IO ProofResult (Int,OHist,String)
oprove_nht t tl (ng,h,indent) = case getResult h tl of
   Just r -> do
      putStrLn (indent ++ show r ++ " previously: "
                ++ showOTL t tl)
      return (r, (ng,h,indent))
   Nothing -> do
      putStrLn (indent ++ "To Prove: " ++ showOTL t tl)
      (r, (ng',h',_)) <-
         (t |-- tl) oprove_nht (ng, h, ".  " ++ indent)
      putStrLn (indent ++ show r ++ ": " ++ showOTL t tl)
      return (r, (ng' + 1, addProof h' tl r, indent))
\end{code}


\verb"oprove_nhl t tl (0,h)" returns \verb"(r,(ng,h'))",
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}, {\tt ng} is the
number of subgoals required to do so, {\tt h} is
a history of prior results and \verb"h'" is the final
history. This prover avoids redoing prior proofs, and
performs loop checking.

\begin{code}
oprove_nhl :: OTheory -> Tagged OLiteral
	    -> ThreadedTest Maybe ProofResult (Int, OHist)
oprove_nhl t tl (ng,h) = case getResult h tl of
   Just Pending ->
      return (Bottom, (ng, addProof h tl Bottom))
   Just r ->
      return (r, (ng, h))
   Nothing -> do
      (r, (ng',h')) <-
          (t |-- tl) oprove_nhl (ng, addProof h tl Pending)
      return (r, (ng' + 1, addProof h' tl r))
\end{code}


\verb+oprove_nhlt t tl (0,h,"")+ returns
\verb+(r,(ng,h',""))+,
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}, {\tt ng} is the
number of subgoals required to do so, {\tt h} is
a history of prior results and \verb"h'" is the final
history. This prover avoids redoing prior proofs, and
performs loop checking.

\begin{code}
oprove_nhlt
   :: OTheory -> Tagged OLiteral
      -> ThreadedTest IO ProofResult (Int, OHist, String)
oprove_nhlt t tl (ng,h,indent) = case getResult h tl of
   Just Pending -> do
      putStrLn (indent ++ "Loop detected: "
                ++ showOTL t tl)
      return (Bottom, (ng, addProof h tl Bottom, indent))
   Just r -> do
      putStrLn (indent ++ show r ++ " previously: "
                ++ showOTL t tl)
      return (r, (ng, h, indent))
   Nothing -> do
      putStrLn (indent ++ "To Prove: " ++ showOTL t tl)
      (r, (ng',h',_)) <-
         (t |-- tl) oprove_nhlt
	    (ng, addProof h tl Pending, ".  " ++ indent)
      putStrLn (indent ++ show r ++ ": " ++ showOTL t tl)
      return (r, (ng' + 1, addProof h' tl r, indent))
\end{code}


\submodule{Provers with array histories} %%%%%%%%%%%%%%%%%

The tree implementation of histories works well, but
adds changes the complexity of a proof with $N$ subgoals
from $O(N)$ to $O(N \log N)$. This can be avoided by
replacing the tree in the history by an array. Accessing
and {\it updating} the array must however be performed in
constant time or there will be no speedup. This requires
mutable arrays, and therefore the ST monad.

We must record the results for each possible tagged
literal. This is essentially a three dimensional structure,
$\{+. -\} \times \{ \Delta, \partial, \ldots\}
\times$ literals.

These declarations define a collection of parallel
mutable arrays that hold all possible proof results.

\begin{code}
type LitHist s = STArray s OLiteral ProofResult
\end{code}

\begin{code}
type SyLitHist s = Array ProofSymbol (LitHist s)
\end{code}

\begin{code}
type PmSyLitHist s = (SyLitHist s, SyLitHist s) -- (+,-)
\end{code}


Between proofs we need frozen (immutable) versions.

\begin{code}
type FLitHist = Array OLiteral ProofResult
\end{code}

\begin{code}
type FSyLitHist = Array ProofSymbol FLitHist
\end{code}

\begin{code}
type FPmSyLitHist = (FSyLitHist, FSyLitHist) -- (+,-)
\end{code}

\begin{code}
type FHist = FPmSyLitHist -- F = flat and frozen
\end{code}


An initial history takes some building.

\begin{code}
initLitHist :: OTheory -> FLitHist
initLitHist t =
   listArray (bounds $ facts t) (repeat NotAttempted)
\end{code}

\begin{code}
initSyLitHist :: FLitHist -> FSyLitHist
initSyLitHist flh =
   listArray (PS_D, PS_dt) (repeat flh)
\end{code}

\begin{code}
initPmSyLitHist :: OTheory -> FPmSyLitHist
initPmSyLitHist t =
   let flh  = initLitHist t
       fslh = initSyLitHist flh
   in (fslh, fslh)
\end{code}


{\tt extendPmSyLitHist ot fh} rebuilds the history
{\tt fh} as new literals are introduced by a new
optimized theory {\tt ot}. For the moment, we'll
just reset it.

\begin{code}
extendPmSyLitHist ::
   OTheory -> FPmSyLitHist -> FPmSyLitHist
extendPmSyLitHist t (p,m) =
   initPmSyLitHist t
\end{code}

A the start of a proof, we must thaw the history.

\begin{code}
thawLitHist :: FLitHist -> ST s (LitHist s)
thawLitHist = thaw
\end{code}

\begin{code}
thawSyLitHist :: FSyLitHist -> ST s (SyLitHist s)
thawSyLitHist a = do
   let as = elems a
   as' <- mapM thawLitHist as
   return $ listArray (bounds a) as'
\end{code}

\begin{code}
thawPmSyLitHist :: FPmSyLitHist -> ST s (PmSyLitHist s)
thawPmSyLitHist (p,m) = do
   p' <- thawSyLitHist p
   m' <- thawSyLitHist m
   return (p', m')
\end{code}


At the end of a proof, we must freeze the history.

\begin{code}
freezeLitHist :: LitHist s -> ST s FLitHist
freezeLitHist = freeze
\end{code}

\begin{code}
freezeSyLitHist :: SyLitHist s -> ST s FSyLitHist
freezeSyLitHist a = do
   let as = elems a
   as' <- mapM freezeLitHist as
   return $ listArray (bounds a) as'
\end{code}

\begin{code}
freezePmSyLitHist :: PmSyLitHist s -> ST s FPmSyLitHist
freezePmSyLitHist (p,m) = do
   p' <- freezeSyLitHist p
   m' <- freezeSyLitHist m
   return (p', m')
\end{code}


\verb"oprove_nH t tl (0,h)" returns \verb"(r,(ng,h'))",
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}, {\tt ng} is the
number of subgoals required to do so, {\tt h} is
a history of prior results and \verb"h'" is the final
history. This prover avoids redoing prior proofs, but
does not perform loop checking.

\begin{code}
oprove_nH :: OTheory -> Tagged OLiteral
   -> ThreadedTest (ST s) ProofResult (Int, PmSyLitHist s)
oprove_nH t tl (ng,(p,m)) = do
   r <- case tl of
      Plus  ps q -> readArray (p ! ps) q
      Minus ps q -> readArray (m ! ps) q
   case r of
      NotAttempted -> do
         (r', (ng',(p',m')))
	    <- (t |-- tl) oprove_nH (ng,(p,m))
	 case tl of
            Plus  ps q -> writeArray (p' ! ps) q r'
            Minus ps q -> writeArray (m' ! ps) q r'
	 return (r', (ng' + 1, (p',m')))
      _ ->
         return (r, (ng, (p, m)))
\end{code}


\verb"oprove_nHl t tl (0,h)" returns \verb"(r,(ng,h'))",
where {\tt r} is the result of trying to prove tagged
literal {\tt tl} with theory {\tt t}, {\tt ng} is the
number of subgoals required to do so, {\tt h} is
a history of prior results and \verb"h'" is the final
history. This prover avoids redoing prior proofs, and
performs loop checking.

\begin{code}
oprove_nHl :: OTheory -> Tagged OLiteral
   -> ThreadedTest (ST s) ProofResult (Int, PmSyLitHist s)
oprove_nHl t tl (ng,(p,m)) = do
   r <- case tl of
      Plus  ps q -> readArray (p ! ps) q
      Minus ps q -> readArray (m ! ps) q
   case r of
      Pending -> do
	 case tl of
            Plus  ps q -> writeArray (p ! ps) q Bottom
            Minus ps q -> writeArray (m ! ps) q Bottom
	 return (Bottom, (ng + 1, (p,m)))
      NotAttempted -> do
         (r', (ng',(p',m')))
	    <- (t |-- tl) oprove_nHl (ng,(p,m))
	 case tl of
            Plus  ps q -> writeArray (p' ! ps) q r'
            Minus ps q -> writeArray (m' ! ps) q r'
	 return (r', (ng' + 1, (p',m')))
      _ ->
         return (r, (ng, (p, m)))
\end{code}


\submodule{Prover selector} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\verb"oprove ls t ot options def tl h fh" uses the prover
selected by the {\tt e} option in {\tt options}, or
the default indicated by {\tt def} if the {\tt e}
option is not present, to prove {\tt tl} using {\tt ot}.
{\tt h} is a tree history of prior results.
{\tt fh} is a flat (array) history of prior results.
If the literal in
{\tt tl} is not defined in the present optimized theory,
i.e. not in {\tt ls}, a new one is built to accommodate it.
An updated history, literal name set, optimized theory
and the proof result as a string are returned.

\begin{code}
oprove :: SparseSet Literal -> Theory -> OTheory
   -> Options -> String -> Tagged Literal
   -> OHist -> FHist
   -> IO (SparseSet Literal, OTheory, OHist, FHist, String)
oprove ls t ot options def tl h fh = do
      let tls = getLits tl emptySS
          (ls', ot', fh')
              = if tls `isSubSet` ls then
                  (ls, ot, fh)
               else
                  let ns  = tls `unionSS` ls
		      ot' = makeOTheory ns t
		      fh'' = extendPmSyLitHist ot' fh
                  in (ns, ot', fh'')
          otl = makeOTL ot' tl
      case lookupBST "e" options of
         Nothing -> oprove ls' t ot' (updateBST (\x _ -> x)
                       "e" (ParamValue def) options)
                       def tl h fh'
         Just (ParamValue cs) -> do
            (h',fh'',r) <- case cs of
               "-"    -> use_prove_     ot' otl fh'
               "n"    -> use_prove_n    ot' otl fh'
               "t"    -> use_prove_t    ot' otl fh'
               "nt"   -> use_prove_nt   ot' otl fh'
               "nh"   -> use_prove_nh   ot' otl fh'
               "nht"  -> use_prove_nht  ot' otl fh'
               "nhl"  -> use_prove_nhl  ot' otl fh'
               "nhlt" -> use_prove_nhlt ot' otl fh'
               "nH"   -> use_prove_nH   ot' otl fh'
               "nHl"  -> use_prove_nHl  ot' otl fh'
               _      -> do
                  putStrLn $ "Error: No such prover as \""
	                     ++ cs ++ "\""
	          return (h, fh', "")
            return (ls', ot', h', fh'', r)
   where
   use_prove_ ot otl fh' = do
      time0 <- getCPUTime
      let Just (result,_) = oprove_ ot otl ()
      putStrLn $ show result ++ "."
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, fh', show result)
   use_prove_n ot otl fh' = do
      time0 <- getCPUTime
      let Just (result,ng) = oprove_n ot otl 0
      putStrLn $ show result ++ "."
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, fh', show result)
   use_prove_t ot otl fh' = do
      time0 <- getCPUTime
      (result,_) <- oprove_t ot otl ""
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, fh', show result)
   use_prove_nt ot otl fh' = do
      time0 <- getCPUTime
      (result,(ng,_)) <- oprove_nt ot otl (0, "")
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, fh', show result)
   use_prove_nh ot otl fh' = do
      time0 <- getCPUTime
      let Just (result,(ng,h')) = oprove_nh ot otl (0,h)
      putStrLn $ show result ++ "."
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h', fh', show result)
   use_prove_nht ot otl fh' = do
      time0 <- getCPUTime
      (result,(ng,h',_)) <- oprove_nht ot otl (0,h,"")
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h', fh', show result)
   use_prove_nhl ot otl fh' = do
      time0 <- getCPUTime
      let Just (result,(ng,h')) = oprove_nhl ot otl (0,h)
      putStrLn $ show result ++ "."
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h', fh', show result)
   use_prove_nhlt ot otl fh' = do
      time0 <- getCPUTime
      (result,(ng,h',_)) <- oprove_nhlt ot otl (0,h,"")
      putStrLn $ "Number of goals: " ++ show ng
      time1 <- getCPUTime
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h', fh', show result)
   use_prove_nH ot otl fh' = do
      time0 <- getCPUTime
      let (result,(ng,fh'')) = runST (do
                h <- thawPmSyLitHist fh'
                (result,(ng,h')) <- oprove_nH ot otl (0,h)
	        fh''' <- freezePmSyLitHist h'
	        return (result,(ng,fh'''))
	     )
      putStrLn $ show result ++ "."
      time1 <- getCPUTime
      putStrLn $ "Number of goals: " ++ show ng
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, fh'', show result)
   use_prove_nHl ot otl fh' = do
      time0 <- getCPUTime
      let (result,(ng,fh'')) = runST (do
                h <- thawPmSyLitHist fh'
                (result,(ng,h')) <- oprove_nHl ot otl (0,h)
	        fh''' <- freezePmSyLitHist h'
	        return (result,(ng,fh'''))
	     )
      putStrLn $ show result ++ "."
      time1 <- getCPUTime
      putStrLn $ "Number of goals: " ++ show ng
      putStrLn $ "CPU time for proof (s): "
         ++ show (fromIntegral(time1 - time0) / 1.0e12)
      return (h, fh'', show result)
\end{code}
