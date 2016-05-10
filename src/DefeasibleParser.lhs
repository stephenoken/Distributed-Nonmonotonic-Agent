\module{DefeasibleParser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

See the user's guide (section~\ref{defParserUser}) for
a description of this module.

\begin{code}
module Main (main) where
\end{code}

\begin{code}
import System.IO
import System.Environment  
\end{code}

\begin{code}
import ABR.Parser
import ABR.Control.Check
import ABR.Parser.Checks
\end{code}

\begin{code}
import DefeasibleLexer; import DTheory
\end{code}

\begin{code}
main :: IO ()
main = do
   paths <- getArgs
   if null paths
      then do
         source <- getContents
         parse source
      else run paths
\end{code}

\begin{code}
run :: [FilePath] -> IO ()
run = mapM_ run1
\end{code}

\begin{code}
run1 :: FilePath -> IO ()
run1 path = do
   putStr $ "Theory file name: " ++ path ++ "\n"
   source <- readFile path
   parse source
\end{code}

\begin{code}
parse :: String -> IO ()
parse source = do
   case checkParse lexerL (total theoryP) source of
      CheckFail msg  -> putStrLn msg
      CheckPass t -> do
	 putStrLn "\nParsed OK.\n"
	 putStrLn $ show t
	 case cyclesCheck t of
            CheckFail msg ->
	       putStrLn $ "\nCycles in priorities: " ++ msg
	    CheckPass t'  -> do
	       putStrLn "\nNo cyclic priorities. \
	                 \Grounding variables:\n"
	       case groundCheck t of
	          CheckFail msg -> putStrLn msg
		  CheckPass t''  -> putStr $ show t''
\end{code}
