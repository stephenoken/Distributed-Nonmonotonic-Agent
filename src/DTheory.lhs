\module{Theories} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The module {\tt DTheory} defines the Defeasible logic
theory data types.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances,FlexibleInstances #-}
\end{code}

\begin{code}
module DTheory(
      LabeledRule(..), LRule, DTheory(..), Theory,
      PrologTheory(..), DeloresTheory(..), theoryP,
      cyclesCheck, groundCheck
   ) where
\end{code}

\begin{code}
import Data.List
import DebugUtils.Trace
\end{code}

\begin{code}
import ABR.Data.BSTree; import ABR.SparseSet
import ABR.Parser; import ABR.Showing; import ABR.List
import ABR.Control.Check
\end{code}

\begin{code}
import Label; import Literal; import Priority
import DRule
\end{code}

\submodule{Data type definitions} %%%%%%%%%%%%%%%%%%%%%%%%

A Defeasible theory consists of a set of facts (literals),
a set of rules (some of which may be labeled),
and a priority relation. These parameterized type
definitions make possible some fancy multi-parameter
class definitions later on.

\begin{code}
data LabeledRule lit = Rule !Label !(DRule lit)
                       deriving (Eq)
\end{code}

\begin{code}
data DTheory rul = Theory [Literal] [rul] [Priority]
                   deriving (Eq)
\end{code}

For shorthand use:

\begin{code}
type LRule = LabeledRule Literal
type Theory = DTheory LRule
\end{code}

A {\tt Statement} is an intermediate data
structure used while parsing.

\begin{code}
data Statement =   Fact        !Literal
                 | LabeledRule !LRule
                 | Priority    !Priority
                 | Superiority !Rule !Rule deriving (Show)
\end{code}

The wrapper types {\tt PrologTheory} and
{\tt PrologPriority} are used to mark
theories and priorities for Prolog syntax output.

\begin{code}
newtype PrologTheory = PrologTheory Theory
\end{code}

\begin{code}
data PrologPriority = !Rule :>> !Rule
\end{code}

The wrapper types {\tt DeloresTheory} and
{\tt DeloresRule} are used to mark
theories and rules for \delores\ syntax output.

\begin{code}
newtype DeloresTheory = DeloresTheory Theory
\end{code}

\begin{code}
newtype DeloresRule = DeloresRule LRule
\end{code}


\submodule{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Syntax:

\input{\texInc theorySyntax.tex}

Implemented:

\begin{code}
rule'P :: Parser Rule
rule'P = prologRuleP <|> ruleP
\end{code}

\begin{code}
prologSuperiorityP :: Parser Statement
prologSuperiorityP
   = literalP "name1" "sup"
     ABR.Parser.*> nofail (literalP "symbol" "(")
     ABR.Parser.*> nofail (literalP "symbol" "(")
     ABR.Parser.*> nofail' "rule expected" rule'P
     ABR.Parser.<*> (nofail (literalP "symbol" ")")
          ABR.Parser.*> nofail (literalP "symbol" ",")
          ABR.Parser.*> nofail (literalP "symbol" "(")
          ABR.Parser.*> nofail' "rule expected" rule'P
          ABR.Parser.<* nofail (literalP "symbol" ")")
          ABR.Parser.<* nofail (literalP "symbol" ")"))
     @> (\(r1,r2) -> Superiority r1 r2)
\end{code}

\begin{code}
factP :: Parser Literal
factP = prologLiteralP <|> pLiteralP
\end{code}

\begin{code}
labeledRuleP :: Parser (LabeledRule Literal)
labeledRuleP = optional (labelP ABR.Parser.<* literalP "symbol" ":")
               ABR.Parser.<*> rule'P
	       @> (\(ls,r) -> case ls of
	             []  -> Rule (Label "") r
		     [l] -> Rule l r
	          )
\end{code}

\begin{code}
statementP :: Parser Statement
statementP =     prologSuperiorityP
             <|> labeledRuleP @> LabeledRule
             <|> factP        @> Fact
             <|> priorityP    @> Priority
\end{code}

\begin{code}
theoryP :: Parser Theory
theoryP
   = trace ("In DTheory.lhs Line: 150 in theoryP ") total (many (statementP ABR.Parser.<* nofail (
                                  literalP "symbol" ".")))
     @> makeTheory
     where
     makeTheory :: [Statement] -> Theory
     makeTheory = trace ("In DTheory.lhs Line: 155 in makeTheory ") (\(fs,rs,ps) -> Theory fs rs ps)
                  . pass2 0 . pass1
     pass1 :: [Statement]
        -> ([Literal], [LRule], [Priority], [(Rule,Rule)])
     pass1 []
        = ([],[],[],[])
     pass1 (s:ss)
        = case trace ("In DTheory.lhs Line: 155 in pass1 s: " ++ show s ++ " ss: " ++ show ss) pass1 ss of
             (fs,rs,ps,sups) ->
                case s of
                   Fact f ->
		      ((f : fs), rs, ps, sups)
                   LabeledRule r ->
		      (fs, (r : rs), ps, sups)
                   Priority p ->
		      (fs, rs, (p : ps), sups)
                   Superiority r1 r2 ->
		      (fs, rs, ps, (r1,r2) : sups)
     pass2 :: Int
        -> ([Literal], [LRule], [Priority], [(Rule,Rule)])
        -> ([Literal], [LRule], [Priority])
     pass2 _ (fs, rs, ps, [])
        = trace ("In DTheory.lhs Line: 176 in pass2  " ++ show (fs, rs, ps)) (fs, rs, ps)
     pass2 n (fs, rs, ps, ((r1,r2):sups))
        = case trace ("In DTheory.lhs Line: 178 in pass2  " ++ show (fs, rs, ps, ((r1,r2):sups))) findRule r1 rs n of
             (l1, rs', n') ->
                case trace ("In DTheory.lhs Line: 181 in findRule  r2 = " ++ show r2 ++ " rs'= " ++ show rs') findRule r2 rs' n' of
                   (l2, rs'', n'') ->
                      pass2 n'' (fs, rs'', (l1 :> l2) : ps,
		                 sups)
     findRule
        :: Rule -> [LRule] -> Int -> (Label, [LRule], Int)
     findRule _ [] _
        = error "rule in sup relation does not exist in \
	        \theory."
     findRule r' ((Rule label r):rs) n
        | r' /= r
           = case trace ("In DTheory.lhs Line: 185 in findRule r' " ++ show r') findRule r' rs n of
                (l, rs', n') ->
                   (l, (Rule label r) : rs', n')
        | otherwise
           = case trace ("In DTheory.lhs Line: 185 in findRule label= " ++ show label) label of
                Label "" ->
                   let l = Label $ "R__" ++ show n
                   in (l, (Rule l r) : rs, n + 1)
                _ ->
                   (label, (Rule label r) : rs, n)
\end{code}


\submodule{Checking for cycles} %%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt cyclesCheck t} detects cycles in the priority
relation of theory {\tt t}. The theory is returned
passed or, on failure, the showed list of priorities
involved in cycles is returned.

\begin{code}
cyclesCheck :: Check Theory Theory String
cyclesCheck t@(Theory _ _ ps)
   = case  trace ("In DTheory.lhs Line: 213 in cyclesCheck Params ps =  " ++ show ps) cycles ps of
        []  -> trace ("In DTheory.lhs Line: 213 in cyclesCheck Params t =  " ++ show t) CheckPass t
	ps' -> trace ("In DTheory.lhs Line: 213 in cyclesCheck Params ps' =  " ++ show ps') CheckFail $ show ps'
\end{code}


\submodule{Labeled rule manipulations} %%%%%%%%%%%%%%%%%%%

{\tt dropLabel lr} converts a labeled rule {\tt lr} to a
{\tt Rule}.

\begin{code}
dropLabel :: LRule -> Rule
dropLabel (Rule _ r) = r
\end{code}


\submodule{Grounding all variables} %%%%%%%%%%%%%%%%%%%%%%

The {\tt groundCheck} passes a theory if it can
replace all facts and rules with ground instances
generated from the constants appearing in the theory.
If there are variables, but no constants the check fails.

\begin{code}
groundCheck :: Check Theory Theory String
groundCheck t@(Theory fs rs ps)
   = let cs = trace ("In DTheory.lhs Line 240  in groundCheck " ++ show t) flattenSS $ getConstNames t emptySS
         vs = getVarNames t emptySS
         fs' = concat $ map (groundAll cs) fs
	 rs' = concat $ map (groundAll cs) rs
	 renumber :: Int -> [LRule] -> ([LRule],
	    BSTree Label (SparseSet Label))
	 renumber n rs = case rs of
	    []                         ->
	       ([], emptyBST)
	    ((Rule (Label "") r) : rs) ->
	       let (rs', t) = renumber (n+1) rs
	       in (Rule (Label ("R" ++ show n)) r : rs', t)
	    ((Rule l r) : rs)          ->
	       let (rs', t) = renumber (n+1) rs
	           l' = Label ("R" ++ show n)
		   sl' = insertSS l' emptySS
	       in (Rule l' r : rs',
	           trace ("In DTheory.lhs Line: 240 in groundCheck Params l =  " ++ show l ++ " sl'= " ++ show sl' ++ " t= " ++ show t) updateBST unionSS l sl' t)
	 (rs'', lmap) = renumber 0 rs'
	 dupPri :: Priority -> [Priority]
	 dupPri (l :> l')
	    = let Just lS = lookupBST l lmap
	          ls = flattenSS lS
		  Just lS' = lookupBST l' lmap
	          ls' = flattenSS lS'
              in [l :> l' | l <- ls, l' <- ls']
	 ps' = concat $ map dupPri ps
     in if nullSS vs && not (null cs) then
           CheckFail "Can't ground variables. \
	             \No constants."
        else
	   CheckPass (Theory fs' rs'' ps')
\end{code}


\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

Textual output.

\begin{code}
instance Show LRule where
\end{code}

\begin{code}
   showsPrec p (Rule l r)
      = case l of
           Label "" -> shows r
           _        -> shows l . showString ": " . shows r
\end{code}

\begin{code}
instance Show Theory where
\end{code}

\begin{code}
   showsPrec p (Theory fs rs ps)
      = showWithTerm ".\n" fs . showWithTerm ".\n" rs
        . showWithTerm ".\n" ps
\end{code}

\begin{code}
instance Show PrologPriority where
\end{code}

\begin{code}
   showsPrec p (r1 :>> r2)
      = showString "sup((" . shows (PrologRule r1)
        . showString "), (" . shows (PrologRule r2)
	. showString "))"
\end{code}

\begin{code}
instance Show PrologTheory where

   showsPrec p (PrologTheory (Theory fs rs ps))
      = showString header
        . showWithTerm ".\n" (map PrologLiteral fs)
	. showWithTerm ".\n" (map pr rs)
	. showWithTerm ".\n" (map pp ps)
        where
	header :: String
	header = "% declarations needed for Sicstus 3\n\
                 \:- multifile (neg)/1, (:=)/2, (:^)/2.\n\
                 \:- dynamic (neg)/1, (:=)/2, (:^)/2.\n\n"
        tree :: BSTree Label Rule
        tree = foldr (\(Rule l r) -> trace("In DTheory.lhs line 326 in tree Params l= " ++  show l ++ " r= " ++ show r) updateBST (\x _ -> x) l r) emptyBST rs
	pr :: LRule -> PrologRule
	pr = PrologRule . dropLabel
	pp :: Priority -> PrologPriority
	pp (l1 :> l2)
	   = case lookupBST l1 tree of
	        Just r1 -> case lookupBST l2 tree of
		   Just r2 -> r1 :>> r2
\end{code}

\begin{code}
instance Show DeloresRule where
\end{code}

\begin{code}
   showsPrec p (DeloresRule (Rule l r))
      = case l of
           Label "" -> shows (PrologRule r)
           _        -> shows l . showString ": "
	               . shows (PrologRule r)
\end{code}

\begin{code}
instance Show DeloresTheory where
\end{code}

\begin{code}
   showsPrec p (DeloresTheory (Theory fs rs ps))
      = showWithTerm ".\n" (map PrologLiteral fs)
	. showWithTerm ".\n" (map DeloresRule rs)
	. showWithTerm ".\n" ps
	. showString "infer.\n"
\end{code}

Extracting literal names.

\begin{code}
instance HasLits LRule where
\end{code}

\begin{code}
   getLits (Rule _ r) = getLits r
\end{code}

\begin{code}
instance HasLits Theory where
\end{code}

\begin{code}
   getLits (Theory fs rs ps) t
      = foldr getLits (foldr getLits t fs) rs
\end{code}

Extracting constant names.

\begin{code}
instance HasConstNames LRule where
\end{code}

\begin{code}
   getConstNames (Rule _ r) = getConstNames r
\end{code}

\begin{code}
instance HasConstNames Theory where
\end{code}

\begin{code}
   getConstNames (Theory fs rs ps) t
      = foldr getConstNames (foldr getConstNames t fs) rs
\end{code}

Extracting variable names.

\begin{code}
instance HasVarNames LRule where
\end{code}

\begin{code}
   getVarNames (Rule _ r) = getVarNames r
\end{code}

\begin{code}
instance HasVarNames Theory where
\end{code}

\begin{code}
   getVarNames (Theory fs rs ps) t
      = foldr getVarNames (foldr getVarNames t fs) rs
\end{code}

Extracting Label names.

\begin{code}
instance HasLabelNames LRule where
\end{code}

\begin{code}
   getLabelNames (Rule l _)
      = getLabelNames l
\end{code}

\begin{code}
instance HasLabelNames Theory where
\end{code}

\begin{code}
   getLabelNames (Theory _ rs ps) t
      = foldr getLabelNames (foldr getLabelNames t rs) ps
\end{code}

{\tt LabeledRule}s are still rules:

\begin{code}
instance IsRule LabeledRule Literal where
\end{code}

\begin{code}
   isStrict    = isStrict    . dropLabel
   isPlausible = isPlausible . dropLabel
   isDefeater  = isDefeater  . dropLabel
   antecedent  = antecedent  . dropLabel
   consequent  = consequent  . dropLabel
\end{code}

Grounding.

\begin{code}
instance Groundable LRule where
\end{code}

\begin{code}
   ground v c (Rule l r) = Rule l (ground v c r)
\end{code}
