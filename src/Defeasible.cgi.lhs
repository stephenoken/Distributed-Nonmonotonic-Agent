\module{CGI Tool} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This module implements the CGI tool that provides a
web interface for the \deimos\ system.
Section~\ref{CGIUser} describes its use.

\begin{code}
module Main (main) where
\end{code}

\begin{code}
import System.Directory; import Data.List; import Data.Char
\end{code}

\begin{code}
import ABR.CGI; import ABR.Control.Check
import ABR.Data.BSTree; import ABR.SparseSet
import ABR.Parser hiding (cons); import ABR.Parser.Checks
import ABR.Text.Markup
\end{code}

\begin{code}
import Literal; import DTheory; import DefeasibleLexer
import History; import DInference; import ODTheory
\end{code}

\submodule{Paths} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\label{cgiPathsSection}

These constants will require modification to set \deimos\
up on new web servers. Use new values of {\tt installWhere}
to select the right values.
{\tt infoDir} is a file path to a directory containing
some texts to be included in the output. {\tt theoryDir}
is the path to the sample theories.
{\tt infoURL} is the URL that gets to same directory
pointed to by {\tt infoDir}.
{\tt theoryURL} is the URL that gets to same directory
pointed to by {\tt theoryDir}.

\begin{code}
installWhere :: String
installWhere = "kurango"
\end{code}

\begin{code}
infoDir, theoryDir ::  FilePath
infoDir = if installWhere == "hunchentoot"
   then
      "/Program Files/Apache Group/Apache/htdocs/def-info/"
   else if installWhere == "kurango" then
      "doc/"
   else
      "doc/"
theoryDir = if installWhere == "hunchentoot"
   then
      "/Program Files/Apache Group/Apache/htdocs/\
      \def-theories/"
   else if installWhere == "kurango" then
      "theories/"
   else
      "theories/"
\end{code}

\begin{code}
infoURL, theoryURL :: String
infoURL = if installWhere == "hunchentoot"
   then
      "http://localhost/def-info/"
   else if installWhere == "kurango" then
      "doc/"
   else
      "doc/"
theoryURL = if installWhere == "hunchentoot"
   then
      "http://localhost/def-theories/"
   else if installWhere == "kurango" then
      "theories/"
   else
      "theories/"
\end{code}

{\tt subs~text} prints {\tt text} replacing all occurrences
of \verb"###I###" with the value of {\tt infoURL},
\verb"###T###" by {\tt theoryURL}, and \verb"###C###" by
the CGI tool URL. This permits included HTML documents to
refer back to the tool and information directories.

\begin{code}
subs :: String -> IO ()
subs css =
   if css == "" then
     return ()
   else if take 7 css == "###I###" then do
      putStr infoURL
      subs (drop 7 css)
   else if take 7 css == "###T###" then do
      putStr theoryURL
      subs (drop 7 css)
   else if take 7 css == "###C###" then do
      script <- getSCRIPT_NAME
      putStr script
      subs (drop 7 css)
   else do
      putChar $ head css
      subs (tail css)
\end{code}

\submodule{Main entry point} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
main :: IO ()
main = do
   printMimeHeader
   queryString <- getQUERY_STRING
   case queryString of
      ""             -> doWelcome
      "new-theory"   -> doNewTheory
      "theory"       -> doTheory
      "proof"        -> doProof
      "syntax"       -> doSyntax
      "proof-help"   -> doProofHelp
      _              -> doBadQuery
\end{code}


\submodule{Common cosmetic bits} %%%%%%%%%%%%%%%%%%%%%%%%%

{\tt wrap~title~rows} prints the HTML code common to
every page. The content of each page must be a sequence
of table rows. Each page has a title.

\begin{code}
wrap :: String -> [IO ()] -> IO ()
wrap title rows = htmlN (do
      headN $ titleN $ put $ "Deimos: " ++ title
      bodyE [("background",
              infoURL ++ "background.jpg")] (do
            centerN $ h1N $ fontE [("color", "FFFFFF")] (do
                  iN $ put "Deimos"
                  put ": "
                  put $ title
               )
            tableE [("cellpadding","10"),
                    ("cellspacing","10"),
                    ("width","100%")] $ sequence_ rows
            imgE_ [("src", infoURL ++ "logo.jpg")]
         )
   )
\end{code}

{\tt row color item} prints item in a table data
element in a row with the given background colour.
{\tt norm item} displays an item in a row with the
normal background colour. {\tt high} displays the item
in a highlight background color. {\tt oops item} displays
the item in a row with an error-indicating background
colour.
\verb"oops'" displays  a plain text message in a PRE
element. \verb"whoops" does all that and puts it in
a complete document with a title.

\begin{code}
row :: String -> IO () -> IO ()
row colour item = trE [("bgcolor",colour)] $ tdN item
\end{code}

\begin{code}
norm, high, oops :: IO () -> IO ()
norm = row "FFFFFF"
high = row "FFFF99"
oops = row "FF9999"
\end{code}

\begin{code}
oops' :: String -> IO ()
oops' = oops . preN . put
\end{code}

\begin{code}
whoops :: String -> String -> IO ()
whoops title = wrap title . (: []) . oops'
\end{code}

{\tt form~query~items} produces a form with
{\tt query} as the URL query string and containing the
form elements in {\tt items}

\begin{code}
form :: String -> IO () -> IO ()
form query items = do
   script <- getSCRIPT_NAME
   formE [("method","post"),
          ("action",script ++ "?" ++ query)] items
\end{code}

{\tt link~query~text} produces a hyperlink back to this
CGI tool with {\tt query} as the URL query string and
containing the {\tt text}.

\begin{code}
link :: String -> IO () -> IO ()
link query text = do
   script <- getSCRIPT_NAME
   aE [("href",script ++ "?" ++ query)] text
\end{code}

This item is displayed when the query string in the
URL is not understood.

\begin{code}
doBadQuery :: IO ()
doBadQuery = wrap "Unknown Query String" [
      oops $ pN $ put "The query string in the URL is \
                      \unknown."
   ]
\end{code}


\submodule{Welcome} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt doWelcome} shows the entry page for the system, which
includes context information, a selection of example
theories, and a link to a page where new theories may be
entered.

\begin{code}
doWelcome :: IO ()
doWelcome = wrap
   "Query Answering Defeasible Logic System" [
      high introMsg,
      high pickATheory,
      high newTheory
   ]
\end{code}

\begin{code}
introMsg :: IO ()
introMsg = do
   text <- readFile $ infoDir ++ "intro.html"
   subs text
\end{code}

\begin{code}
pickATheory :: IO ()
pickATheory = form "theory" (do
      h2N $ put "Select an Example Defeasible Theory"
      fileNames <- getDirectoryContents theoryDir
      let fileNames' = sort $ filter ((== 't') . head
                       . reverse) fileNames
      pN $ put "Click on an example:"
      pN $ selectE [("name","theory"), ("size","20")]
         $ mapM_ theoryOption fileNames'
      pN $ inputE_ [("name","origin"), ("type","hidden"),
                    ("value","file")]
      pN $ inputE_ [("name","submit"), ("type","submit"),
                    ("value","Open Theory")]
   )
\end{code}

\begin{code}
theoryOption :: FilePath -> IO ()
theoryOption file = do
   contents <- readFile $ theoryDir ++ "/" ++ file
   optionE [("value",file)] $ put $ trim $ drop 1
      $ trim $ head $ lines $ contents
\end{code}

\begin{code}
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace
       . reverse
\end{code}

\begin{code}
newTheory :: IO ()
newTheory = do
   h2N $ put "Create a New Defeasible Theory"
   pN (do
      put "Click "
      link "new-theory" $ put "here"
      put " to create a new defeasible theory."
     )
\end{code}


\submodule{New theory} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt doNewTheory} displays the page with the form
where new theories may be typed in.

\begin{code}
doNewTheory :: IO ()
doNewTheory = wrap "New Theory" [high $ form "theory" (do
      pN (do
            put "Type in your new theory. (The syntax \
	        \for theories is defined "
            link "syntax" $ put "here"
            put ".)"
            inputE_ [("name","origin"),
	             ("type","hidden"),("value", "form")]
         )
      pN $ textareaE [("name","theory"), ("cols","80"),
        ("rows","15"), ("wrap","off")] $ return ()
      pN $ inputE_ [("type","submit"), ("name","submit"),
         ("value","Go Prove Things")]
   )]
\end{code}


\submodule{Theory} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt doTheory} displays the page where the theory is
displayed and queries are prompted for.

\begin{code}
doTheory :: IO ()
doTheory = do
   let title = "Defeasible Theory"
   formData <- getFormData
   lookupGuard  formData ["origin","theory"]
      (\ cs -> whoops title $ "Missing " ++ cs ++ ".")
      (\ [origin,theory] -> do
         source <- if origin == "file" then
                      readFile $ theoryDir ++ theory
                   else
                      return theory
         wrap title [
               high $ showTheory source,
               case (emptyCheck "theory"
                  &? checkParse lexerL (total theoryP)
                  &? cyclesCheck) source  of
                  CheckFail msg -> oops' msg
                  CheckPass th  -> high $ queryForm source
            ]
      )
\end{code}

\begin{code}
emptyCheck :: String -> Check String String String
emptyCheck item content =
   if and $ map isSpace content then
      CheckFail $ "The " ++ item ++ " is empty."
   else
      CheckPass content
\end{code}

\begin{code}
showTheory :: String -> IO ()
showTheory t = do
   h2N $ put "Defeasible Theory"
   tableE [("cellpadding","10"), ("cellspacing","10"),
	   ("width","100%")] $ norm $ preN $ put t
\end{code}

\begin{code}
queryForm :: String -> IO ()
queryForm th = form "proof" (do
      h2N $ put "Do a Proof"
      inputE_ [("name","theory"), ("type","hidden"),
         ("value", makeHTMLSafe th)]
      pN (do
            put "What do you want to prove? "
            inputE_ [("name","taggedliteral"),
                     ("type","text"), ("size","15")]
            put " ("
            link "proof-help" $ put "What do I type here?"
            put ")"
         )
      pN (do
            put "Select a prover with: "
            selectE [("name","prover")] (do
                  opt "-"    "no extras"
                  opt "n"    g
                  opt "nh"   (g +++ h)
                  opt "nhl"  (g +++ h +++ l)
                  opt "t"    t
                  opt "nt"   (g +++ t)
                  opt "nht"  (g +++ h +++ t)
                  optionE [("value","nhlt"),
                     ("selected","")] $ put
                     (g +++ h +++ l +++ t)
                  opt "nH"   (g +++ h')
                  opt "nHl"  (g +++ h' +++ l)
               )
         )
      pN $ inputE_ [("type","submit"), ("name","submit"),
	 ("value","Prove it")]
   )
   where
   opt name name'
      = optionE [("value",name)] $ put name'
   g = "goal counting"
   h = "history keeping"
   h' = "faster history keeping"
   l = "loop detection"
   t = "tracing"
   (+++) a b = a ++ ", " ++ b
\end{code}


\submodule{Proof} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt doProof} displays the page containing the results
of a query.

\begin{code}
doProof :: IO ()
doProof = do
   let title = "Proof"
   formData <- getFormData
   lookupGuard formData ["theory", "taggedliteral",
      "prover"]
      (\ cs -> whoops title $ "Missing " ++ cs ++ ".")
      (\ [t,tl, p] -> wrap title
                   $ proveIt (noSemicolons t) tl p)
\end{code}

\begin{code}
noSemicolons :: String -> String
noSemicolons cs = case cs of
   []                       -> []
   (';':';':';':';':';':cs) -> '\n' : noSemicolons cs
   (c:cs)                   -> c : noSemicolons cs
\end{code}

\begin{code}
proveIt :: String -> String -> String -> [IO ()]
proveIt source q prover =
   high (showTheory source) :
   case (emptyCheck "theory"
      &? checkParse lexerL (nofail theoryP)
      &? cyclesCheck &? groundCheck) source  of
      CheckFail msg -> [oops' msg]
      CheckPass t ->
         showQuery q :
         case (emptyCheck "query" &? checkParse lexerL
            (total taggedLiteralP) &? checkNoVars) q of
            CheckFail msg -> [oops' msg]
            CheckPass tl  -> [high (do
                  h2N $ put "Proof"
                  proveIt' t tl prover
               )]
\end{code}

\begin{code}
showQuery :: String -> IO ()
showQuery q = high (do
      h2N $ put "Query (Tagged Literal)"
      tableE [("cellpadding","10"), ("cellspacing","10"),
	      ("width","100%")] $ norm $ preN $ put q
   )
\end{code}

\begin{code}
proveIt' :: Theory -> Tagged Literal -> String -> IO ()
proveIt' t tl prover = do
   let s = getLits tl (getLits t emptySS)
       ot = makeOTheory s t
   tableE [("cellpadding","10"), ("cellspacing","10"),
	   ("width","100%")] $ norm (do
	  putStr "<pre>"
          (_,_,_,_,r) <- oprove s t ot emptyBST prover tl
               emptyHistory (initPmSyLitHist ot)
	  putStr "</pre>"
	  h3N $ put r
      )
\end{code}


\submodule{Help pages} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt doSyntax} and {\tt doProofHelp} display the
help pages for this CGI tool.

\begin{code}
doSyntax :: IO ()
doSyntax = wrap "Syntax" [norm (do
      text <- readFile $ infoDir ++ "syntax.html"
      subs text
   )]
\end{code}

\begin{code}
doProofHelp :: IO ()
doProofHelp = wrap "Proof Help" [norm (do
      text <- readFile $ infoDir ++ "proof-help.html"
      subs text
   )]
\end{code}
