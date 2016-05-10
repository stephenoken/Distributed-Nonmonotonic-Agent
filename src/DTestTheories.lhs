\module{Scalable Test Theories}

This module defines functions that generate scalable
test Defeasible theories and queries to exercise them.

\begin{code}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
\end{code}

\begin{code}
module DTestTheories(
      generateTheory, generateTL, generateMetrics
   ) where
\end{code}

\begin{code}
import Literal; import DRule; import Label
import Priority; import DTheory; import DInference
\end{code}

\begin{code}
infix 7 >>>
\end{code}

\submodule{Shorthand} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Scalable theories are usually built with
literals of the form $a_{i}$. {\tt a~i} returns
such a literal. {\tt na~i} returns the corresponding
negative literal $\neg a_{i}$.

\begin{code}
a, na :: Int -> Literal
a  i = PosLit ('a' : show i)
na i = NegLit ('a' : show i)
\end{code}

Theories are built from (usually) labeled rules.
{\tt r~i~rule} adds a label to {\tt rule}. The label
is a capital {\tt R} followed by {\tt i}.

\begin{code}
r :: Int -> Rule -> LRule
r i = Rule (Label ('R' : show i))
\end{code}

Priorities indicate one rule beats another.
{\tt r1~>>>~r2} returns a priority $r_{1} > r_{2}$.
This operator is overloaded. Priorities can be
made from label numbers, labels or labeled rules.

\begin{code}
class MakesPriority a where

   (>>>) :: a -> a -> Priority

instance MakesPriority Int where

   i >>> j
      = (Label ('R' : show i)) :> (Label ('R' : show j))

instance MakesPriority Label where

   (>>>) = (:>)

instance MakesPriority LRule where

   (Rule l1 _) >>> (Rule l2 _) = (l1 :> l2)
\end{code}


\submodule{Chain theories} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\label{chainTheoriesImpl}

See section~\ref{chainTheories} for a description of
chain theories.

{\tt chainTheory n} returns theory
\chainTh . {\tt chainTL n} returns the
default tagged literal $+\partial a_{n}$ the proof
of which exercises all of theory \chainTh .

\begin{code}
chainTheory :: Int -> Theory
chainTheory n
   = Theory
        [a 0] [r i ([a (i-1)] :=> a i) | i <- [1..n]] []

chainTL :: Int -> Tagged Literal
chainTL n = Plus PS_d (a n)
\end{code}

{\tt chainSTheory n} returns theory
\chainSTh\ which is a strict variant
of \chainTh . {\tt chainSTL n} returns the
default tagged literal $+\Delta a_{n}$.

\begin{code}
chainSTheory :: Int -> Theory
chainSTheory n
   = Theory
        [a 0] [r i ([a (i-1)] :-> a i) | i <- [1..n]] []

chainSTL :: Int -> Tagged Literal
chainSTL n = Plus PS_D (a n)
\end{code}

{\it Testing note}: Space friendly, $O(1)$ stack,
$O(1)$ heap (over theory storage). Can generate
$10^{6}$ rules in Mac Hugs with default heap and
stack.


\submodule{Circle theories} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\label{circleTheoriesImpl}

See section~\ref{circleTheories} for a description of
circle theories.

{\tt circleTheory n} returns theory
\circleTh . {\tt circleTL n} returns the
default tagged literal $+\partial a_{0}$ the proof
of which exercises all of theory \circleTh .

\begin{code}
circleTheory :: Int -> Theory
circleTheory n
   = Theory
        [] [r i ([a i] :=> a ((i+1) `mod` n))
	   | i <- [0..n-1]] []

circleTL :: Tagged Literal
circleTL = Plus PS_d (a 0)
\end{code}

{\tt circleSTheory n} returns theory
\circleSTh\ which is a strict variant
of \circleTh . {\tt circleSTL n} returns the
default tagged literal $+\Delta a_{0}$.

\begin{code}
circleSTheory :: Int -> Theory
circleSTheory n
   = Theory
        [] [r i ([a i] :-> a ((i+1) `mod` n))
	   | i <- [0..n-1]] []

circleSTL :: Tagged Literal
circleSTL = Plus PS_D (a 0)
\end{code}

{\it Testing note}: Space friendly, $O(1)$ stack,
$O(1)$ heap (over theory storage).


\submodule{Levels theories} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\label{levelsTheoriesImpl}

See section~\ref{levelsTheories} for a description of
levels theories.

{\tt levelsTheory n} returns theory
\levelsTh . {\tt levelsTL n} returns the
default tagged literal $+\partial a_{0}$ the proof
of which exercises all of theory \levelsTh .

\begin{code}
levelsTheory :: Int -> Theory
levelsTheory n
   = Theory [] (rules (-1)) (priorities 0)
     where
     rules i
        | i < 0
           =   (r 0 ([] :=> a 0)) : rules (i+1)
        | i <= n
           =   (r (4*i+1) ([a (2*i+1)] :=> na (2*i)))
             : (r (4*i+2) ([]          :=> a (2*i+1)))
             : (r (4*i+3) ([a (2*i+2)] :=> na (2*i+1)))
             : (r (4*i+4) ([]          :=> a (2*i+2)))
             : rules (i + 1)
        | otherwise
           = []
     priorities i
        | i < 0     = priorities (i+1)
        | i <= n    =   (4*i+3) >>> (4*i+2)
	              : priorities (i+1)
        | otherwise = []

levelsTL :: Tagged Literal
levelsTL = Plus PS_d (a 0)
\end{code}

{\tt levels\_Theory n} returns theory
\levelsMTh\ which is a variant
of \levelsTh\ that omits the priorities.
{\tt levels\_TL n} returns the
default tagged literal $+\partial a_{0}$.

\begin{code}
levels_Theory :: Int -> Theory
levels_Theory n
   = let Theory fs rs _ = levelsTheory n
     in Theory fs rs []

levels_TL :: Tagged Literal
levels_TL = Plus PS_d (a 0)
\end{code}

{\it Testing note}: Space friendly, $O(1)$ stack,
$O(1)$ heap (over theory storage). Can generate
$10^{6}$ rules in Mac Hugs with default heap and
stack.


\submodule{Teams theories} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\label{teamsTheoriesImpl}

See section~\ref{teamsTheories} for a description of
teams theories, \teamsTh .

\begin{code}
teamsTheory :: Int -> Theory
teamsTheory n = Theory [] (rules 0 0) (priorities 0 0)
   where
   rules :: Int -> Int -> [LRule]
   rules i t  -- i = level, t = # rules in prior levels
      | i < n     = tRules 0
      | otherwise = bRules 0
      where
      k = 4 ^ (i + 1) -- # rules at level i
      bRules j        -- bottom level rules
         | j < k     =   r (t + j) ([] :=> c j)
	               : bRules (j + 1)
	 | otherwise = []
      tRules j        -- top and middle level rules
         | j < k     =   r (t + j) ([a j] :=> c j)
	               : tRules (j + 1)
	 | otherwise = rules (i + 1) (t + k)
      c j = (if j `mod` 4 < 2 then PosLit else NegLit)
            ('a' : show ((t + j) `div` 4))
      a j = PosLit ('a' : show (t + 1 + j))
   priorities :: Int -> Int -> [Priority]
   priorities i t
      | i < n     = tPriors 0
      | otherwise = bPriors 0
      where
      k = 4 ^ (i + 1)
      bPriors j
         | j < k     =   (t + j)     >>> (t + j + 2)
	               : (t + j + 1) >>> (t + j + 3)
	               : bPriors (j + 4)
	 | otherwise = []
      tPriors j
         | j < k     =   (t + j)     >>> (t + j + 2)
	               : (t + j + 1) >>> (t + j + 3)
	               : tPriors (j + 4)
	 | otherwise = priorities (i + 1) (t + k)

teamsTL :: Tagged Literal
teamsTL  = Plus PS_d (a 0)
\end{code}

{\it Testing note}: Space friendly, $O(1)$ stack,
$O(1)$ heap (over theory storage). Can generate
$10^{6}$ rules ($n = 9$) in Mac Hugs.


\submodule{Tree theories} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\label{treeTheoriesImpl}

See section~\ref{treeTheories} for a description of
tree theories, \treeTh .

\begin{code}
treeTheory :: Int -> Int -> Theory
treeTheory n k = Theory facts rules []
   where
   facts = [a i | let above = sum [k^j | j <- [0..n-1]],
                  i <- [above .. above + k * k^(n-1) - 1]]
   rules = [r i (as :=> a i) | d <- [0..n-1],
            w <- [0..k^d-1],
            let above = sum [k^j | j <- [0..d-1]]
                i = above + w
                below = above + k^d + w * k
                as = [a j | j <- [below .. below + k - 1]]]

treeTL :: Tagged Literal
treeTL = Plus PS_d (a 0)
\end{code}

{\it Testing note}: Space friendly, $O(1)$ stack,
$O(1)$ heap (over theory storage). Can generate
$10^{6}$ rules ($n = 12, k = 3$) in Mac Hugs.


\submodule{Directed acyclic graph theories} %%%%%%%%%%%%%%

\label{dagTheoriesImpl}

See section~\ref{dagTheories} for a description of
directed acyclic graph theories, \dagTh .

\begin{code}
dagTheory :: Int -> Int -> Theory
dagTheory n k = Theory facts rules []
   where
   facts = [a i | i <- [k*n+1..k*n+k]]
   rules = [r i (as :=> a i) | i <- [0..k*n],
            let as = [a (i+j) | j <- [1..k]]]

dagTL :: Tagged Literal
dagTL= Plus PS_d (a 0)
\end{code}

{\it Testing note}: Space friendly, $O(1)$ stack,
$O(1)$ heap (over theory storage). Can generate
$10^{6}$ rules in Mac Hugs.


\submodule{Mix theories} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\label{mixTheoriesImpl}

See section~\ref{mixTheories} for a description of
directed mix theories, \mixTh .

\begin{code}
mixTheory :: Int -> Int -> Int -> Theory
mixTheory m n k
   = Theory facts rules []
     where
     p = PosLit "p"
     np = NegLit "p"
     a i j = PosLit $ "a" ++ show i ++ "_" ++ show j
     b i j k = PosLit $ "b" ++ show i ++ "_" ++ show j
               ++ "_" ++ show k
     facts
        | k == 0
           = [a i j | i <- [1..2*m], j <- [1..n]]
        | otherwise
           = [b i j 1 | i <- [1..2*m], j <- [1..n]]
     rules
        = rules' 1 1 1 0
     rules' i j k' l
        | i > 2 * m
           = []
        | j > n
           = (if i <= m
                 then (r l ([a i j | j <- [1..n]] :=> p))
                 else (r l ([a i j | j <- [1..n]] :~> np))
             )
             : rules' (i+1) 1 1 (l+1)
        | k' > k
           = rules' i (j+1) 1 l
        | k' == k
           = (r l ([b i j k] :-> a i j))
             : rules' i j (k'+1) (l+1)
        | otherwise
           = (r l ([b i j k'] :-> b i j (k'+1)))
             : rules' i j (k'+1) (l+1)

mixTL :: Tagged Literal
mixTL = Plus PS_d (PosLit "p")
\end{code}

{\it Testing note}: Space friendly, $O(1)$ stack,
$O(1)$ heap (over theory storage). Can generate
$10^{6}$ rules in Mac Hugs.


\submodule{Selectors} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt generateTheory~name~sizes} returns the named theory.
{\tt sizes} is a list of size parameters to select
the size of the theory.

\begin{code}
generateTheory :: String -> [Int] -> Maybe Theory
generateTheory name sizes
   | name == "chain"
       = if head sizes >= 0
            then Just $ chainTheory $ head sizes
            else Nothing
   | name == "chains"
       = if head sizes >= 0
            then Just $ chainSTheory $ head sizes
            else Nothing
   | name == "circle"
       = if head sizes >= 0
            then Just $ circleTheory $ head sizes
            else Nothing
   | name == "circles"
       = if head sizes >= 0
            then Just $ circleSTheory $ head sizes
            else Nothing
   | name == "levels"
       = if head sizes >= 0
            then Just $ levelsTheory $ head sizes
            else Nothing
   | name == "levels-"
       = if head sizes >= 0
            then Just $ levels_Theory $ head sizes
            else Nothing
   | name == "teams"
       = if head sizes >= 0
            then Just $ teamsTheory $ head sizes
            else Nothing
   | name == "tree"
       = if length sizes == 2 && and (map (> 0) sizes)
            then let [n,k] = sizes
	         in Just $ treeTheory n k
            else Nothing
   | name == "dag"
       = if length sizes == 2 && and (map (> 0) sizes)
            then let [n,p] = sizes
	         in Just $ dagTheory n p
            else Nothing
   | name == "mix"
       = if length sizes == 3 && and (map (>= 0) sizes)
            then let [m,n,k] = sizes
	         in Just $ mixTheory m n k
            else Nothing
   | otherwise
       = Nothing
\end{code}

{\tt generateTL~name~sizes} returns the suggested
tagged literal to prove for the named theory.
{\tt sizes} is a list of size parameters to select
the size of the theory.

\begin{code}
generateTL :: String -> [Int] -> Maybe (Tagged Literal)
generateTL name sizes
   | name == "chain"   = Just $ chainTL $ head sizes
   | name == "chains"  = Just $ chainSTL $ head sizes
   | name == "circle"  = Just $ circleTL
   | name == "circles" = Just $ circleSTL
   | name == "levels"  = Just levelsTL
   | name == "levels-" = Just levels_TL
   | name == "teams"   = Just teamsTL
   | name == "tree"    = Just treeTL
   | name == "dag"     = Just dagTL
   | name == "mix"     = Just mixTL
   | otherwise         = Nothing
\end{code}

{\tt generateMetrics~name~sizes} computes the tuple
$(facts, rules, priorities, size)$ which contains
the metrics computed for the {\tt name}d theory with the
given {\tt sizes}.

\begin{code}
generateMetrics :: String -> [Int]
   -> Maybe (Int, Int, Int, Int)
generateMetrics name sizes
   | name == "chain" =
      let n = head sizes
      in Just (
         1,
	 n,
	 0,
	 2 * n + 1
      )
   | name == "chains" =
      let n = head sizes
      in Just (
         1,
	 n,
	 0,
	 2 * n + 1
      )
   | name == "circle" =
      let n = head sizes
      in Just (
         0,
	 n,
	 0,
	 2 * n
      )
   | name == "circles" =
      let n = head sizes
      in Just (
         0,
	 n,
	 0,
	 2 * n
      )
   | name == "levels" =
      let n = head sizes
      in Just (
         0,
	 4 * n + 5,
	 n + 1,
	 7 * n + 8
      )
   | name == "levels-" =
      let n = head sizes
      in Just (
         0,
	 4 * n + 5,
	 0,
	 6 * n + 7
      )
   | name == "teams" =
      let n = head sizes
      in Just (
         0,
	 4 * sum [4 ^ i | i <- [0..n]],
	 2 * sum [4 ^ i | i <- [0..n]],
	 10 * sum [4 ^ i | i <- [0..n-1]] + 6 * (4^n)
      )
   | name == "tree" =
      let n = head sizes
          k = sizes !! 1
      in Just (
         k^n,
	 sum [k ^ i | i <- [0..n-1]],
	 0,
	 (k+1) * sum [k ^ i | i <- [0..n-1]] + k^n
      )
   | name == "dag" =
      let n = head sizes
          k = sizes !! 1
      in Just (
         k,
	 n * k + 1,
	 0,
	 n * (k^2) + (n+2) * k + 1
      )
   | name == "mix" =
      let m = head sizes
          n = sizes !! 1
          k = sizes !! 2
      in Just (
         2 * m * n,
	 2 * m + 2 * m * n * k,
	 0,
	 2 * m + 4 * m * n + 4 * m * n * k
      )
   | otherwise = Nothing
\end{code}
