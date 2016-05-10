\module{Rules} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module {\tt DRule} implements a data type for
representing rules in Defeasible logic theories.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, 
             TypeSynonymInstances #-}
\end{code}

\begin{code}
module DRule (
      DRule(..), Rule, PrologRule(..), ruleP, prologRuleP,
      IsRule(..)
   ) where
\end{code}

\begin{code}
import ABR.Parser; import ABR.Showing
\end{code}

\begin{code}
import Literal
\end{code}

\begin{code}
infix 4 :->, :=>, :~>
\end{code}

\submodule{Data type definitions} %%%%%%%%%%%%%%%%%%%%%%%%

These data type declarations are suitable for easy
manipulation of rules and as parse trees. This definition
is parameterized with respect to the type of literal
to be used. This makes this code a little more general,
and makes possible some fancy stuff with multi-parameter
type classes later on.

\begin{code}
data DRule lit =   ![lit] :-> !lit
                 | ![lit] :=> !lit
                 | ![lit] :~> !lit
                 deriving (Eq, Ord)
\end{code}

As shorthand, use this type synonym.

\begin{code}
type Rule = DRule Literal
\end{code}

To mark a rule for Prolog output, wrap up in this
type.

\begin{code}
newtype PrologRule = PrologRule Rule
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The syntax for a rule is:

\input{\texInc ruleSyntax.tex}

\noindent which is implemented:

\begin{code}
antecedentP :: Parser [Literal]
antecedentP 
   =     literalP "symbol" "{" <*> literalP "symbol" "}"
         #> []
     <|> literalP "symbol" "{"
         *> (pLiteralP <*>
	     many (literalP "symbol" "," *> pLiteralP))
         <* nofail (literalP "symbol" "}")
         @> cons
     <|> pLiteralP <*>
         many (literalP "symbol" "," *> pLiteralP)
         @> cons
     <|> epsilonA
         #> []
\end{code}

\begin{code}
ruleP :: Parser Rule
ruleP = antecedentP
        <*> (    literalP "symbol" "->"
	     <|> literalP "symbol" "=>"
	     <|> literalP "symbol" "~>") <*> pLiteralP
	@> (\(as,((_,arrow,_),c)) -> (case arrow of
	         "->" -> (:->)
	         "=>" -> (:=>)
	         "~>" -> (:~>)
	      ) as c)
\end{code}

The alternate \dprolog-compatible syntax for a rule is:

\input{\texInc prologRuleSyntax.tex}

\noindent which is implemented:

\begin{code}
prologAntecedentP :: Parser [Literal]
prologAntecedentP 
   =     literalP "name1" "true"
         #> []
     <|> prologLiteralP
         <*> many (literalP "symbol" ","
	           *> nofail' "literal expected"
		              prologLiteralP)
         @> cons
\end{code}

\begin{code}
prologRuleP :: Parser Rule
prologRuleP
   = prologLiteralP
     <*> (    literalP "symbol" ":-"
	  <|> literalP "symbol" ":="
	  <|> literalP "symbol" ":^")
     <*> prologAntecedentP
     @> (\(c,((_,arrow,_),as)) -> (case arrow of
             ":-" -> (:->)
	     ":=" -> (:=>)
	     ":^" -> (:~>)
	  ) as c)
\end{code}


\submodule{Properties of rules} %%%%%%%%%%%%%%%%%%%%%%%%%%

The {\tt IsRule} class collects the properties of
rules and rule-like types. {\tt is{\it X} r} returns 
{\tt True} iff {\tt r} is an {\tt\it X}. 
{\tt antecedent r} returns the list of literals
which are the antecedents of rule {\tt r}. 
{\tt consequent r} returns the literal which is the
consequent of {\tt r}. This is a multi-parameter
type class, which relies on Haskell extensions.

\begin{code}
class IsRule rul lit where
\end{code}

\begin{code}
   isStrict :: rul lit -> Bool
\end{code}

\begin{code}
   isPlausible :: rul lit -> Bool
\end{code}

\begin{code}
   isDefeater :: rul lit -> Bool
\end{code}

\begin{code}
   antecedent :: rul lit -> [lit]
\end{code}

\begin{code}
   consequent :: rul lit -> lit
\end{code}

\begin{code}
instance IsRule DRule Literal where
\end{code}

\begin{code}
   isStrict r = case r of
                   _ :-> _ -> True
                   _       -> False
\end{code}

\begin{code}
   isPlausible r = case r of
                      _ :=> _ -> True
                      _       -> False
\end{code}

\begin{code}
   isDefeater r = case r of
                     _ :~> _ -> True
                     _       -> False
\end{code}

\begin{code}
   antecedent r = case r of
                     a :-> _ -> a
                     a :=> _ -> a
                     a :~> _ -> a
\end{code}

\begin{code}
   consequent r = case r of
                     _ :-> c -> c
                     _ :=> c -> c
                     _ :~> c -> c
\end{code}


\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

Conversion of rules to printable representations
is implemented by declaring these instances of
class {\tt Show}.

\begin{code}
instance Show Rule where
\end{code}

\begin{code}
   showsPrec p rule = case rule of 
         (a :-> c) -> showsAntecedent a . showString " -> "
                      . shows c
         (a :=> c) -> showsAntecedent a . showString " => "
	              . shows c
         (a :~> c) -> showsAntecedent a . showString " ~> "
	              . shows c
      where
      showsAntecedent as = showWithSep ", " as
\end{code}

\begin{code}
instance Show PrologRule where
\end{code}

\begin{code}
   showsPrec p (PrologRule rule) = case rule of 
         (a :-> c) -> shows (PrologLiteral c)
                      . showString " :- " 
		      . showsAntecedent a
         (a :=> c) -> shows (PrologLiteral c)
                      . showString " := "
		      . showsAntecedent a
         (a :~> c) -> shows (PrologLiteral c) 
                      . showString " :^ " 
		      . showsAntecedent a
      where
      showsAntecedent as = case as of
	 [] -> showString "true"
         _  -> showWithSep ", " $ map PrologLiteral as
\end{code}

Introducing type {\tt Rule} to class {\tt HasLits} enables 
the extraction of all the unique literals in a rule.

\begin{code}
instance HasLits Rule where
\end{code}

\begin{code}
   getLits r t = case r of 
      (a :-> c) -> foldr getLits (getLits c t) a
      (a :=> c) -> foldr getLits (getLits c t) a
      (a :~> c) -> foldr getLits (getLits c t) a
\end{code}

Extracting constant names.

\begin{code}
instance HasConstNames Rule where
\end{code}

\begin{code}
   getConstNames r t = case r of 
      (a :-> c) ->
         foldr getConstNames (getConstNames c t) a
      (a :=> c) ->
         foldr getConstNames (getConstNames c t) a
      (a :~> c) ->
         foldr getConstNames (getConstNames c t) a
\end{code}

Extracting variable names.

\begin{code}
instance HasVarNames Rule where
\end{code}

\begin{code}
   getVarNames r t = case r of 
      (a :-> c) -> foldr getVarNames (getVarNames c t) a
      (a :=> c) -> foldr getVarNames (getVarNames c t) a
      (a :~> c) -> foldr getVarNames (getVarNames c t) a
\end{code}

Grounding.

\begin{code}
instance Groundable Rule where
\end{code}

\begin{code}
   ground v c r = case r of 
      qs :-> q -> map (ground v c) qs :-> ground v c q 
      qs :=> q -> map (ground v c) qs :=> ground v c q 
      qs :~> q -> map (ground v c) qs :~> ground v c q 
\end{code}
