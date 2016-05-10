

% Configs.lhs





% This file was produced from Configs.lit


% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2007, 2008,  Andrew Rock
% 
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

\module{Text.Configs} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Text.Configs} provides a type, parser
and pretty printer for a sequence of configuration
settings, as might be found in a configuration file.
This kind of data could be stored in XML, but this format
is nicer to edit by hand.

\begin{code}
module ABR.Text.Configs (
      Config(..), Configs, configsL, configsP, stringL,
      showConfigs, read', lookupConfig, updateConfig,
      lookupParam, getParam, popTemplate
   ) where
\end{code}


\begin{code}
import Data.Char; import Data.List
\end{code}

\begin{code}
import ABR.Parser.Pos; import ABR.Parser
import ABR.Parser.Lexers; import ABR.Text.String
import ABR.Showing
\end{code}



\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2009-04-13: Changed to {\tt ABR.\emph{Text}.Configs};
   added design for templates; refactored un/enString out
   to {\tt ABR.Text.String}.



\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A configuration, \highlighttt{Config}, is one of:

\begin{description}
   \item[\highlighttt{CFlag}] a flag that is set by its
      presence;
   \item[\highlighttt{CParam}] a parameter with an
      associated value;
   \item[\highlighttt{CSet}] a parameter with an
      associated set of configurations; or
   \item[\highlighttt{CList}] a parameter with an
      associated list of configurations.
\end{description}

\begin{code}
data Config =   CFlag String
              | CParam String String
              | CSet String Configs
              | CList String [Configs]
              deriving (Eq, Ord)
\end{code}

\noindent A \highlighttt{Configs} is a list of
configurations.

\begin{code}
type Configs = [Config]
\end{code}

\submodule{Lexer} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Comments in configuration files start with \verb+#+ and
extend to the end of the line. Comments are treated as
whitespace. There can be any amount of whitespace between
tokens. Aside from inside strings, and to separate names,
whitespace is not significant.

\InputEBNF{Configs}{comment}


\begin{code}
commentL :: Lexer
commentL = 
   literalL '#'
   <**> (many (satisfyL (/= '\n') "") *%> "")
   <**> (optional (literalL '\n') *%> "")
   %> " "
\end{code}


\noindent Names in configuration files may contain
letters, digits, plus and minus signs, underscores,
periods and bangs. Note that a number can lex as a {\tt
name}. Names are case sensitive.

\InputEBNF{Configs}{name}


\begin{code}
nameL :: Lexer
nameL = some (satisfyL nameChar "") *%> "name"
   where
   nameChar :: Char -> Bool
   nameChar c = 
      isAlpha c || isDigit c || c `elem` "+-_.!"
\end{code}


\noindent Strings are delimited by double quotes and may
extend across many lines. Use two double quotes for one,
{\it \`{a} la} Pascal. 

\InputEBNF{Configs}{string}

\noindent The other symbols used are:

\begin{description}
   \item[{\tt =}] to bind a name to a value (either a {\tt
      name} or a {\tt string}), configuration set or
      configuration list;
   \item[{\tt \char`\{}] to start a configuration set;
   \item[{\tt \char`\}}] to close a configuration set;
   \item[{\tt [}] to start a configuration list;
   \item[{\tt ]}] to close a configuration list; and
   \item[{\tt ,}] to separate items in a configuration
      list.
\end{description}


\noindent \highlighttt{configsL} is the lexer that will
tokenize a configuration source.

\InputEBNF{Configs}{configsL}

\begin{code}
configsL :: Lexer

configsL = dropWhite $ nofail $ total $ listL [
      commentL, nameL, stringL, 
      tokenL "=" %> "symbol", tokenL "{" %> "symbol",
      tokenL "}" %> "symbol", tokenL "[" %> "symbol",
      tokenL "]" %> "symbol", tokenL "," %> "symbol",
      whitespaceL
   ]

\end{code}

\submodule{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\noindent A {\tt value} is either a {\tt name} or a {\tt
string}.

\InputEBNF{Configs}{value}


\begin{code}
valueP :: Parser String 
valueP = (tagP "name" <|> tagP "string")
         @> (\(_,v,_) -> v)
\end{code}


\noindent A {\tt configs} is a sequence of whitespace
separated 



{\tt config}s, parsed by \highlighttt{configsP}.


\InputEBNF{Configs}{configs}


\begin{code}
configsP :: Parser Configs

configsP = many configP

\end{code}


\noindent A {\tt configSet} is a {\tt configs} in braces.

\InputEBNF{Configs}{configSet}


\begin{code}
configSetP :: Parser Configs

configSetP = 
   literalP "symbol" "{"
   *> nofail' "configs expected" configsP
   <* nofail (literalP "symbol" "}")

\end{code}


\noindent A {\tt configList} is a comma separated sequence
of {\tt configs} in brackets.

\InputEBNF{Configs}{configList}


\begin{code}
configListP :: Parser [Configs]

configListP = 
           literalP "symbol" "["
        *> configsP
       <*> many (
                 literalP "symbol" ","
              *> nofail' "configs expected"
                    configsP
           )
       <*  nofail (literalP "symbol" "]")
       @> cons
   <|>     literalP "symbol" "["
        *> configsP
       <*  nofail (literalP "symbol" "]")
       @> (: [])
   <|>     literalP "symbol" "["
       <*> nofail (literalP "symbol" "]")
       #> []

\end{code}


\noindent A {\tt config} is either: a binding of a name to
a {\tt configSet}, a {\tt configList}, or a {\tt value};
or just a {\tt name}.

\InputEBNF{Configs}{config}


\begin{code}
configP :: Parser Config
configP = 
           tagP "name"
       <*> literalP "symbol" "="
       <*> valueP
       @> (\((_,n,_),(_,v)) -> CParam n v)
   <|>     tagP "name"
       <*> literalP "symbol" "="
       <*> configSetP
       @> (\((_,n,_),(_,cs)) -> CSet n cs)
   <|>     tagP "name"
       <*> literalP "symbol" "="
       <*> nofail configListP
       @> (\((_,n,_),(_,css)) -> CList n css)
   <|> tagP "name"
       @> (\(_,n,_) -> CFlag n)
\end{code}


\submodule{Showing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Show Config where
\end{code}


\begin{code}
    showsPrec p c = case c of
       CFlag n     -> showString n
       CParam n v  -> showString n 
          . showString " = " . showString v
       CSet n cs   -> showString n 
          . showString " = {\n" 
          . showWithTerm "\n" cs
          . showString "}"
       CList n css -> showString n 
          . showString " = [\n"
          . showString (concat (intersperse ",\n" (
               map (unlines . map show) css
            )))
          . showString "]"
\end{code}


\highlighttt{showConfigs}~$\mathit{cs}$ shows a list of
configs.

\begin{code}
showConfigs :: Configs -> String

showConfigs cs = concatMap ((++ "\n") . show) cs

\end{code}

\submodule{Reading} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{read'}~$s$ may be used to read a
parameter value $s$, removing quotes first.

\begin{code}
read' :: Read a => String -> a

read' = read . trim . unString

\end{code}

\submodule{Accessing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{configPaths}

\noindent A \emph{configPath} is a string that selects a {\tt Config}
from within some {\tt Configs}. It can be: the name of a
{\tt Config}, eg {\tt name}; of the form {\tt name.name} to
select from inside a {\tt CSet}; or of the form {\tt
name!digits.name} to select from inside a {\tt CList}; or
of combinations like {\tt class!3.student!7.name.family}.

Note this description. While the names in a {\tt Config}
may be any double-quote-delimited string, those names are
not useable in a configPath.

\subsubmodule{Lookup functions}

\noindent \highlighttt{lookupConfig}~$n~\mathit{cs}$ tries
to find the named config $n$ in $\mathit{cs}$, returning
{\tt Just} the first match or {\tt Nothing}. $n$
is a configPath.

\begin{code}
lookupConfig :: String -> Configs -> Maybe Config

lookupConfig n cs = case cs of
   []                -> 
      Nothing
   CFlag n' : cs     -> 
      if n == n' then 
         Just (CFlag n)
      else 
         lookupConfig n cs
   CParam n' v : cs  -> 
      if n == n' then
         Just (CParam n v)
      else
         lookupConfig n cs
   CSet n' cs' : cs  ->
      if n == n' then
         Just (CSet n cs')
      else if (n' ++ ".") `isPrefixOf` n then
         lookupConfig (drop (length n' + 1) n) cs'
      else
         lookupConfig n cs
   CList n' css : cs -> 
      if n == n' then
         Just (CList n css)
      else if (n' ++ "!") `isPrefixOf` n then
         let (i,n'') = 
                span isDigit (drop (length n' + 1) n)
         in lookupConfig (tail n'') (css !! read i)
      else 
         lookupConfig n cs

\end{code}

\noindent \highlighttt{updateConfig}~$n~c~\mathit{cs}$
tries to replace the named config $n$ in $\mathit{cs}$
with $c$. $n$ is a configPath.
If the named config does not exist it will be created.
THIS IS INCOMPETELY IMPLEMENTED and is the wrong way to build
{\tt Configs} anyway.

\begin{code}
updateConfig :: String -> Config -> Configs -> Configs

updateConfig n c cs = case cs of
   []                ->
      [c] -- does not handle complex names when they need 
          -- to be inserted as lists and sets
   c'@(CFlag n') : cs     -> 
      if n == n' then 
         c : cs
      else 
         c' : updateConfig n c cs
   c'@(CParam n' v) : cs  -> 
      if n == n' then
         c : cs
      else
         c' : updateConfig n c cs
   c'@(CSet n' cs') : cs  ->
      if n == n' then
         c : cs
      else if (n' ++ ".") `isPrefixOf` n then
         CSet n' (updateConfig (drop (length n' + 1) n) c
         cs') : cs
      else
         c' : updateConfig n c cs
   c'@(CList n' css) : cs -> 
      if n == n' then
         c : cs
      else if (n' ++ "!") `isPrefixOf` n then
         let (i,n'') = 
                span isDigit (drop (length n' + 1) n)
             (css',cs':css'') = splitAt (read i) css
         in CList n' (css' ++ updateConfig (tail n'') c cs' 
            : css'') : cs
      else 
         c' : updateConfig n c cs

\end{code}

\noindent \highlighttt{lookupParam}~$n~\mathit{cs}$ tries
to find the named parameter $n$ in $\mathit{cs}$,
returning {\tt Just} the first value or {\tt Nothing}.
(Note {\tt elem} is all that is required to test for a
flag.) $n$ is a configPath.

\begin{code}
lookupParam :: String -> Configs -> Maybe String

lookupParam n cs = case lookupConfig n cs of
   Nothing            -> Nothing
   Just (CParam n' v) -> Just v

\end{code}

\noindent \highlighttt{getParam}~$n~\mathit{cs}$ tries to
find the named parameter. It is an error if the parameter
can not be found. $n$ is a configPath.

\begin{code}
getParam:: String -> Configs -> String

getParam name cs = 
   let mp = lookupParam name cs
   in case mp of
      Nothing -> error $ "Missing config: " ++ name
      Just p  -> p 

\end{code}

\submodule{Templates} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt Configs} are a structured data type like XML and can
be used to populate a template. The power of templates in
part comes from being able to handle variations in the data
that they might get populated with.

\subsubmodule{Simple Template Markup Language}

\begin{itemize}
   \item A template is a {\tt String} containing any kind 
      of text, marked up as HTML or anything else.
      
   \item The hash character (\verb"#") is the special escape 
      character in a template. It it \emph{always} treated
      specially. 
      
   \item To output a hash, use two, eg \verb"##".

   \item The sequence \verb"#"\emph{configPath}\verb"#"
      is replaced in the output by the value of the {\tt
      Config}.
      
   \item The output value of a {\tt Config} is:
   
      \begin{itemize}
         \item \verb"_UNDEFINED_" if the {\tt Config} does
            not exist;
            
         \item \verb"_DEFINED_" if the {\tt Config} is a 
            flag;
         
         \item the unStringed text of a parameter;
         
         \item \verb"_SET_" if the {\tt Config} is a set; 
            or
          
         \item \verb"_LIST_" if the {\tt Config} is a list.
      \end{itemize}
      
   \item In the following sequences, no extra whitespace
      is permitted.
    
   \item The sequence 
      \verb"#ifdef#"\emph{configPath}\verb"#"\emph{text}\verb"#end#"
      outputs the \emph{text} iff \emph{configPath} leads
      to a {\tt Config} of any kind that exists, otherwise
      outputs nothing. The \emph{text} has any template
      markup in it processed as usual.

   \item The sequence 
      \verb"#ifundef#"\emph{configPath}\verb"#"\emph{text}\verb"#end#"
      outputs the \emph{text} iff \emph{configPath} does
      not lead a {\tt Config} that exists, otherwise
      outputs nothing. The \emph{text} has any template
      markup in it processed as usual.

   \item The sequence 
      \verb"#with#"\emph{configPath}\verb"#"\emph{text}\verb"#end#"
      outputs the \emph{text} iff \emph{configPath} leads
      to a set {\tt Config} that exists. The \emph{text}
      has any template markup in it processed using the set
      as the new root {\tt Configs}. If the
      \emph{configPath} does not lead to a {\tt Config}
      that exists, the output is \verb"_UNDEFINED_SET_". If
      the \emph{configPath} does not leads to a {\tt
      Config} that exists, but is not a set, the output is
      \verb"_NOT_A_SET_".

   \item The sequence 
      \verb"#foreach#"\emph{configPath}\verb"#"\emph{text}\verb"#end#"
      outputs the \emph{text} for each element of the list
      iff \emph{configPath} leads to a list {\tt Config}
      that exists. The \emph{text} has any template markup
      in it processed using each element as its root {\tt
      Configs}. If the \emph{configPath} does not lead to a
      {\tt Config} that exists, the output is
      \verb"_UNDEFINED_LIST_". If the \emph{configPath}
      does not leads to a {\tt Config} that exists, but is
      not a list, the output is \verb"_NOT_A_LIST_".
\end{itemize}

\subsubmodule{Populating templates}

\highlighttt{popTemplate}~$\mathit{configs}~\mathit{template}$
returns the text of the $\mathit{template}$ populated with
the data from the $\mathit{configs}$. NOT TESTED

\begin{code}
data Mode = Quiet | Print | Reprint
\end{code}

\begin{code}
popTemplate :: Configs -> String -> String

popTemplate configs = pt [(configs,Print)]
   where
   pt :: [(Configs,Mode)] -> String -> String
   -- every #end# pops a context. in each tuple is the 
   -- current Configs and whether to output
   pt [] "" = ""
   pt (_:_:_) "" = "popTemplate: not enough #end#s."
   pt [] _ = error "popTemplate: too many #end#s."
   pt contexts@((g,m):gvs) template = case template of
      '#':'#':cs -> case m of
         Quiet -> pt contexts cs
         _     -> '#' : pt contexts cs
      '#':'e':'n':'d':'#':cs -> case m of
         Reprint -> ""
         _       -> pt gvs cs
      '#':'i':'f':'d':'e':'f':'#':cs -> 
         case span (/= '#') cs of
            ([],_) -> 
               error "popTemplate: empty path (1)"
            (_,[]) -> 
               error "popTemplate: non-terminated path (1)"
            (path,'#':cs') -> case m of
               Quiet -> pt ((g,Quiet):contexts) cs'
               _     -> case lookupConfig path g of
                  Nothing -> pt ((g,Quiet):contexts) cs'
                  Just _  -> pt ((g,Print):contexts) cs'
      '#':'i':'f':'u':'n':'d':'e':'f':'#':cs -> 
         case span (/= '#') cs of
            ([],_) -> 
               error "popTemplate: empty path (2)"
            (_,[]) -> 
               error "popTemplate: non-terminated path (2)"
            (path,'#':cs') -> case m of
               Quiet -> pt ((g,Quiet):contexts) cs'
               _     -> case lookupConfig path g of
                  Nothing -> pt ((g,Print):contexts) cs'
                  Just _  -> pt ((g,Quiet):contexts) cs'
      '#':'w':'i':'t':'h':'#':cs -> 
         case span (/= '#') cs of
            ([],_) -> 
               error "popTemplate: empty path (3)"
            (_,[]) -> 
               error "popTemplate: non-terminated path (3)"
            (path,'#':cs') -> case m of
               Quiet -> pt ((g,Quiet):contexts) cs'
               _     -> case lookupConfig path g of
                  Nothing -> "_UNDEFINED_SET_" ++ 
                     pt ((g,Quiet):contexts) cs'
                  Just (CSet _ g') ->
                     pt ((g',Print):contexts) cs'
                  Just _ -> "_NOT_A_SET_" ++
                        pt ((g,Quiet):contexts) cs'
      '#':'f':'o':'r':'e':'a':'c':'h':'#':cs -> 
         case span (/= '#') cs of
            ([],_) -> 
               error "popTemplate: empty path (4)"
            (_,[]) -> 
               error "popTemplate: non-terminated path (4)"
            (path,'#':cs') -> case m of
               Quiet -> pt ((g,Quiet):contexts) cs'
               _     -> case lookupConfig path g of
                  Nothing ->
                     "_UNDEFINED_LIST_" ++ 
                        pt ((g,Quiet):contexts) cs'
                  Just (CList _ g') -> case g' of
                     [] -> pt (([],Quiet):contexts) cs'
                     es -> concat [pt 
                           ((e,Reprint):contexts) cs' |
                           e <- init es] ++
                        pt ((last es,Print):contexts) cs' 
                  Just _ ->
                     "_NOT_A_List_" ++
                        pt ((g,Quiet):contexts) cs'
      '#':cs -> 
         case span (/= '#') cs of
            ([],_) -> 
               error "popTemplate: empty path (5)"
            (_,[]) -> 
               error "popTemplate: non-terminated path (5)"
            (path,'#':cs') -> case m of
               Quiet -> pt contexts cs'
               _     -> case lookupConfig path g of
                  Nothing ->
                     "_UNDEFINED_" ++ pt contexts cs'
                  Just (CFlag _) ->
                     "_DEFINED_" ++ pt contexts cs'
                  Just (CParam _ v) ->
                     unString v ++  pt contexts cs'
                  Just (CSet _ _) ->
                     "_SET_" ++ pt contexts cs'
                  Just (CList _ _) ->
                     "_LIST_" ++ pt contexts cs'
      c:cs -> case m of
         Quiet -> pt contexts cs
         _     -> c : pt contexts cs

\end{code}
