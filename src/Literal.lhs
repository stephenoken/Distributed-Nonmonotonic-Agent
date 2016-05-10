\module{Literals} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Literals for the Defeasible and Plausible logic
implementations are defined by module {\tt Literal}.

\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
\end{code}

\begin{code}
module Literal(
      Argument(..), Literal(..), PrologLiteral(..),
      LiteralName, pLiteralP, prologLiteralP, OLiteral,
      LitArray, LitTree, HasLits(..), makeLitTables,
      Negatable(..), IsLiteral(..), HasConstNames(..),
      HasVarNames(..), Subst, Groundable(..)
   ) where
\end{code}

\begin{code}
import Data.Array
\end{code}

\begin{code}
import ABR.Control.Check; import ABR.Data.BSTree
import ABR.SparseSet; import ABR.Showing; import ABR.List
import ABR.DeepSeq; import ABR.Parser
\end{code}

\submodule{Data type definitions} %%%%%%%%%%%%%%%%%%%%%%%%

The primary representation of a literal is a string
containing the name of the literal and a tag that
indicates positive or negative. Some literals in a theory
may have arguments which are either constants or
variables to be replaced by constants.

\begin{code}
type LiteralName  = String
type ConstantName = String
type VariableName = String
\end{code}

\begin{code}
data Argument =   Const ConstantName
                | Var   VariableName
                deriving (Eq, Ord)
\end{code}

\begin{code}
data Literal =   PosLit  LiteralName
               | PosLit_ LiteralName [Argument]
               | NegLit  LiteralName
               | NegLit_ LiteralName [Argument]
               deriving Eq
\end{code}

To mark a literal to be treated as a Prolog literal, for
example to select a different syntax for textual output,
it should be wrapped by the {\tt PrologLiteral}
constructor.

\begin{code}
newtype PrologLiteral = PrologLiteral Literal
\end{code}

After variables have been removed, a literal is just a
constant value. Integers will do. A negative literal is
negative. Zero is not a valid literal since it can not
be negated. Handling integers will be much more
rapid and they can be used as array indices. This type
represents an optimized literal.

\begin{code}
type OLiteral = Int
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The syntax for a literal is:

\input{\texInc literalSyntax.tex}

\noindent which is implemented with these parsers:

\begin{code}
argumentP :: Parser Argument
argumentP = nofail' "argument expected" (
                  tagP "name1" @> (\(_,n,_) -> Const n)
              <|> tagP "name2" @> (\(_,n,_) -> Var n)
	    )
\end{code}

\begin{code}
argListP :: Parser [Argument]
argListP = literalP "symbol" "("
           ABR.Parser.*> argumentP
	   ABR.Parser.<*> many (literalP "symbol" "," ABR.Parser.*> argumentP)
	   ABR.Parser.<* nofail (literalP "symbol" ")")
	   @> cons
\end{code}

\begin{code}
pLiteralP :: Parser Literal
pLiteralP = optional (literalP "symbol" "~")
            ABR.Parser.<*> tagP "name1" ABR.Parser.<*> optional argListP
	    @> (\(ts,((_,n,_),ass)) -> case (ts,ass) of
	          ([],[])    -> PosLit n
		  ([_],[])   -> NegLit n
		  ([],[as])  -> PosLit_ n as
		  ([_],[as]) -> NegLit_ n as
	       )
\end{code}

An alternate syntax for literals, compatible with
\dprolog, is:

\input{\texInc prologLiteralSyntax.tex}

\noindent which is implemented:

\begin{code}
prologLiteralP :: Parser Literal
prologLiteralP
   = optional (literalP "name1" "neg")
     ABR.Parser.<*> tagP "name1" ABR.Parser.<*> optional argListP
     @> (\(negs,((_,n,_),ass)) -> case (negs,ass) of
           ([],[])    -> PosLit n
	   ([_],[])   -> NegLit n
	   ([],[as])  -> PosLit_ n as
	   ([_],[as]) -> NegLit_ n as
	)
\end{code}

\submodule{Negation} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Literals are either positive or negative. The {\tt neg}
function converts from positive to negative and
\textit{vice versa}. This function can be overloaded as
other entities, such as Plausible formulas, can also
be negated. The {\tt Negatable} class includes all such
entities. The {\tt pos} method forces the anything to
be positive.

\begin{code}
class Negatable a where
\end{code}

\begin{code}
   neg :: a -> a
\end{code}

\begin{code}
   pos :: a -> a
\end{code}

\begin{code}
   isPos :: a -> Bool
\end{code}


\begin{code}
instance Negatable Literal where
\end{code}

\begin{code}
   neg l
      = case l of
           PosLit  n    -> NegLit  n
           PosLit_ n as -> NegLit_ n as
           NegLit  n    -> PosLit  n
           NegLit_ n as -> PosLit_ n as
\end{code}

\begin{code}
   pos l
      = case l of
           NegLit  n    -> PosLit  n
           NegLit_ n as -> PosLit_ n as
           _            -> l
\end{code}

\begin{code}
   isPos l
      = case l of
           PosLit  _    -> True
           PosLit_ n as -> True
           _            -> False
\end{code}


\begin{code}
instance Negatable OLiteral where
\end{code}

\begin{code}
   neg = negate
\end{code}

\begin{code}
   pos = abs
\end{code}

\begin{code}
   isPos = (> 0)
\end{code}


\submodule{Literal lookup tables} %%%%%%%%%%%%%%%%%%%%%%%%

\label{classHasLitNames}

The {\tt OLiteral} numeric value that represents the
literal needs to be mapped to and from the
literal. An array lets us map from numbers to literals
in $O(1)$ time. A binary search tree lets us map from
literals to numbers in $O(\log N)$ time, where $N$ is the
number of unique literals.

\begin{code}
type LitArray = Array OLiteral Literal
\end{code}

\begin{code}
type LitTree  = BSTree Literal OLiteral
\end{code}

To build these data structures, we must first collect
all of the unique literals, without distinguishing
positive and negative literals. {\tt getLits thing set}
adds all of the literals in {\tt thing} to {\tt set}.

\begin{code}
class HasLits a where
\end{code}

\begin{code}
   getLits :: a -> SparseSet Literal
              -> SparseSet Literal
\end{code}

\begin{code}
instance HasLits Literal where
\end{code}

\begin{code}
   getLits l = insertSS (pos l)
\end{code}

{\tt makeLitTables set} makes the data structures
required to quickly map between both representations of
literals. {\tt set} is the set of literals
accumulated with {\tt getLits}.

\begin{code}
makeLitTables :: SparseSet Literal -> (LitArray, LitTree)
makeLitTables set =
   let lits = domBST set
       n = length lits
   in (listArray (1,n) lits, pairs2BST (zip lits [1..]))
\end{code}

Using look-up tables created above, literals and some
formulas can be mapped to and from their numeric
equivalents. {\tt toOLiteral tree thing} uses the
{\tt tree} to map {\tt thing} to the equivalent optimized
literal. {\tt fromOLiteral array ol} uses the
{\tt array} to map an optimized literal {\tt ol} to
some other thing which is equivalent. {\tt isLiteral thing}
returns {\tt True} iff {\tt thing} is equivalent to
one literal.

\begin{code}
class IsLiteral a where
\end{code}

\begin{code}
   toOLiteral :: LitTree -> a -> OLiteral
\end{code}

\begin{code}
   fromOLiteral :: LitArray -> OLiteral -> a
\end{code}

\begin{code}
   isLiteral :: a -> Bool
\end{code}

\begin{code}
instance IsLiteral Literal where
\end{code}

\begin{code}
   toOLiteral t l = case l of
      PosLit _ -> case lookupBST l t of
         Just n -> n
	 Nothing -> error "unknown literal"
      NegLit _ -> case lookupBST (pos l) t of
         Just n -> neg n
	 Nothing -> error "unknown literal"
      PosLit_ _ _ -> case lookupBST l t of
         Just n -> n
	 Nothing -> error "unknown literal"
      NegLit_ _ _ -> case lookupBST (pos l) t of
         Just n -> neg n
	 Nothing -> error "unknown literal"
\end{code}

\begin{code}
   fromOLiteral a l
      = let (low, high) = bounds a
            n = abs l
            s = signum l
        in if low <= n && n <= high
             then if s > 0
                     then a ! n
                     else neg (a ! n)
             else error "OLiteral out of range"

   isLiteral l = True
\end{code}


\submodule{Collecting constant names} %%%%%%%%%%%%%%%%%%%%%

To ground all of the removable variables we must first
collect all of the constant names. We can accumulate them
in the same way we can accumulate all of the literal
names.

\begin{code}
class HasConstNames a where
\end{code}

\begin{code}
   getConstNames :: a -> SparseSet ConstantName
                    -> SparseSet ConstantName
\end{code}

\begin{code}
instance HasConstNames Argument where
\end{code}

\begin{code}
   getConstNames a ns = case a of
      Const n -> insertSS n ns
      _       -> ns
\end{code}

\begin{code}
instance HasConstNames Literal where
\end{code}

\begin{code}
   getConstNames l ns = case l of
      PosLit_ _ as -> foldr getConstNames ns as
      NegLit_ _ as -> foldr getConstNames ns as
      _            -> ns
\end{code}


\submodule{Collecting variable names} %%%%%%%%%%%%%%%%%%%%%

To ground all of the removable variables we must first
collect all of the variables names. We can accumulate them
in the same way we can accumulate all of the literal
names.

\begin{code}
class HasVarNames a where
\end{code}

\begin{code}
   getVarNames :: a -> SparseSet VariableName
                    -> SparseSet VariableName
\end{code}

\noindent {\tt hasVars x} returns {\tt True} iff {\tt x}
contains variables.

\begin{code}
   hasVars :: a -> Bool
   hasVars x = nullSS $ getVarNames x emptySS
\end{code}

\noindent {\tt checkNoVars x} is a check that {\tt x} does
{\it not} contain variables.

\begin{code}
   checkNoVars :: Check a a String
   checkNoVars x = if hasVars x
      then CheckPass x
      else CheckFail "Variables are not permitted."
\end{code}


\begin{code}
instance HasVarNames Argument where
\end{code}

\begin{code}
   getVarNames a ns = case a of
      Var n -> insertSS n ns
      _     -> ns
\end{code}

\begin{code}
instance HasVarNames Literal where
\end{code}

\begin{code}
   getVarNames l ns = case l of
      PosLit_ _ as -> foldr getVarNames ns as
      NegLit_ _ as -> foldr getVarNames ns as
      _            -> ns
\end{code}


\submodule{Grounding} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A substitution is a function which performs this
operation. Substitutions may be composed to handle
more than one variable substitution.

\begin{code}
type Subst a = a -> a
\end{code}

To ``ground'' is to substitute a variable with a constant.

\begin{code}
class HasVarNames a => Groundable a where
\end{code}

{\tt ground v c x} returns the thing {\tt x} with all
occurrences of variable {\tt v} replaced by constant
{\tt c}.

\begin{code}
   ground :: VariableName -> ConstantName -> Subst a
\end{code}

{\tt groundAll cs x} returns all of the ground
instances of {\tt x}, obtained by substituting the
constants in {\tt cs} for the variables in {\tt x}.

\begin{code}
   groundAll :: [ConstantName] -> a -> [a]
   groundAll cs x =
      let vs = flattenSS $ getVarNames x emptySS
          nvs = length vs
      in if nvs == 0 then
            [x]
	 else
	    [foldl (.) id (zipWith ground vs cs') x
            | cs' <- cartProd (take nvs (repeat cs))]
\end{code}

\begin{code}
instance Groundable Argument where
\end{code}

\begin{code}
   ground v c a = case a of
      Const c' -> a
      Var   v' -> if v == v' then Const c else a
\end{code}

\begin{code}
instance Groundable Literal where
\end{code}

\begin{code}
   ground v c l = case l of
      PosLit_ n as -> PosLit_ n (map (ground v c) as)
      NegLit_ n as -> NegLit_ n (map (ground v c) as)
      _          -> l
\end{code}


\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

Textual output of literals is performed with the
{\tt show} function, which is a method of class
{\tt Show}.

\begin{code}
instance Show Argument where
\end{code}

\begin{code}
   showsPrec p a
      = case a of
           Const n -> showString n
	   Var   n -> showString n
\end{code}

\begin{code}
instance Show Literal where

   showsPrec p l
      = case l of
           PosLit n ->
	      showString n
           NegLit n ->
	      showChar '~' . showString n
           PosLit_ n as ->
              showString n . showChar '('
	      . showWithSep ", " as . showChar ')'
           NegLit_ n as ->
              showChar '~' . showString n . showChar '('
	      . showWithSep ", " as . showChar ')'
\end{code}

\begin{code}
instance Show PrologLiteral where
\end{code}

\begin{code}
   showsPrec p (PrologLiteral l)
      = case l of
           PosLit n ->
	      showString n
           NegLit n ->
	      showString "neg " . showString n
           PosLit_ n as ->
              showString n . showChar '('
	      . showWithSep ", " as . showChar ')'
           NegLit_ n as ->
              showString "neg " . showString n
	      . showChar '(' . showWithSep ", " as
	      . showChar ')'
\end{code}

\begin{code}
instance Ord Literal where
\end{code}

\begin{code}
   compare q q' = case q of
      PosLit a -> case q' of
         PosLit  b   -> compare a b
         PosLit_ b _ -> if a == b
            then LT
            else compare a b
         NegLit  _   -> GT
         NegLit_ _ _ -> GT
      NegLit a -> case q' of
         PosLit  _   -> LT
         PosLit_ _ _ -> LT
         NegLit  b   -> case compare a b of
            GT -> LT
            EQ -> EQ
            LT -> GT
         NegLit_ b _ -> case compare a b of
            GT -> LT
            EQ -> LT
            LT -> GT
      PosLit_ a ps -> case q' of
         PosLit  b    -> if a == b
            then GT
            else compare a b
         PosLit_ b qs -> if a == b
            then compare ps qs
            else compare a b
         NegLit  _    -> GT
         NegLit_ _ _  -> GT
      NegLit_ a ps -> case q' of
         PosLit  _    -> LT
         PosLit_ _ _  -> LT
         NegLit  b    -> case compare a b of
            LT -> GT
            EQ -> EQ
            GT -> LT
         NegLit_ b qs -> if a == b
            then case compare ps qs of
               LT -> GT
               EQ -> EQ
               GT -> LT
            else case compare a b of
               LT -> GT
               EQ -> EQ
               GT -> LT
\end{code}

\begin{code}
instance DeepSeq Argument where
\end{code}

\begin{code}
   deepSeq a x = case a of
      Const n -> deepSeq n x
      Var   n -> deepSeq n x
\end{code}

\begin{code}
instance DeepSeq Literal where
\end{code}

\begin{code}
   deepSeq l x = case l of
      PosLit  n    -> deepSeq n x
      PosLit_ n as -> deepSeq n $ deepSeq as x
      NegLit  n    -> deepSeq n x
      NegLit_ n as -> deepSeq n $ deepSeq as x
\end{code}
