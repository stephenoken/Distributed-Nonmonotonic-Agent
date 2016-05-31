\module{ODProver} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

See the user's guide (section~\ref{ODProverUser}) for a
description of this module.

\begin{code}
module Main (main) where
\end{code}

\begin{code}
import System.Environment; import System.CPUTime; import Data.Char; import System.IO
import Control.Exception
\end{code}

\begin{code}
import ABR.Args; import ABR.SparseSet
import ABR.Text.String; import ABR.Parser
import ABR.Control.Check; import ABR.Data.BSTree
import ABR.Parser.Checks
\end{code}

\begin{code}
import Literal; import DTheory; import Priority
import DefeasibleLexer; import ODTheory; import History
import DInference
import Debug.Trace
\end{code}

\begin{code}
main :: IO ()
main = do
   args <- getArgs
   run $ unwords args
\end{code}

\begin{code}
run :: String -> IO ()
run args = do
   let (options,others) = trace ("Hello World")
          findOpts [ParamS "e", FlagS "t", FlagS "td",
             FlagS "tp"] (words args)
   case others of
       []   -> getPath options
       p:[] -> openTheory options p Nothing
       p:l  -> openTheory options p (Just (unwords l))
\end{code}

\begin{code}
getPath :: Options -> IO ()
getPath options = do
   putStr "Theory file name (or \"q\" to quit): "
   path <- getLine
   let path' = trim path
   case path' of
      []  -> getPath options
      "q" -> quit
      _:_ -> openTheory options path' Nothing
\end{code}

\begin{code}
openTheory :: Options -> FilePath -> Maybe String -> IO ()
openTheory options path mtl = do
   source <- catch (readFile path) (\(NoMethodError e) -> return "\0")
   case source of
      "\0" -> do
         putStrLn $ "Error: File " ++ path ++ " is \
            \empty or could not be read."
         getPath options
      _ -> case (checkParse lexerL (total theoryP)
                 &? cyclesCheck &? groundCheck) source of
         CheckFail msg -> do
            putStrLn msg
            case mtl of
               Nothing -> getPath options
               _       -> quit
         CheckPass t   -> do
            case (lookupBST "tp" options,
	          lookupBST "td" options) of
	       (Just FlagMinus,_) ->
	          putStr $ show $ PrologTheory t
	       (_,Just FlagMinus) ->
	          putStr $ show $ DeloresTheory t
	       _ -> do
	          let ls = getLits t emptySS
	              ot = makeOTheory ls t
                  case lookupBST "t" options of
	             Just FlagMinus ->
	                putStr $ show ot
	             _ -> case mtl of
                        Nothing    ->
	                   interactive ls t ot options
                        Just l -> do
	                   proveOne ls t ot options l
	                      emptyHistory
	                      (initPmSyLitHist ot)
	                   return ()
\end{code}

\begin{code}
interactive :: SparseSet Literal -> Theory -> OTheory ->
   Options -> IO ()
interactive ls t ot options = do
      putStrLn "Type \"?\" for help."
      proofLoop ls ot options emptyHistory
         (initPmSyLitHist ot)
   where
   proofLoop :: SparseSet Literal -> OTheory
                -> Options -> OHist -> FHist -> IO ()
   proofLoop ls ot options h fh = do
      putStr "|- "
      input <- getLine
      let input' = words input
      case input' of
         [] ->
	    proofLoop ls ot options h fh
         "?" : _ -> do
	    showHelp
	    proofLoop ls ot options h fh
         "q" : _ ->
	    quit
         "t" : _ -> do
	    putStrLn $ show ot
            proofLoop ls ot options h fh
         "td" : _ -> do
	    putStrLn $ show $ DeloresTheory t
            proofLoop ls ot options h fh
         "tp" : _ -> do
	    putStrLn $ show $ PrologTheory t
            proofLoop ls ot options h fh
         "f" : _ -> do
	    putStrLn "Those who forget history \
	             \are destined to repeat it."
            proofLoop ls ot options emptyHistory
	       (initPmSyLitHist ot)
	 "e" : css ->
	    let cs = unwords css
	    in if cs `elem` ["-","n","nh","nhl","nt",
	          "nht","nhlt", "nH", "nHl"] then
	          proofLoop ls ot (updateBST (\x _ -> x)
	          "e" (ParamValue cs) options) h fh
	       else if cs == "" then do
	          putStr "Current prover: "
	          case lookupBST "e" options of
	             Nothing ->
	                putStrLn $ "nhlt"
	             Just (ParamValue p) ->
	                putStrLn p
	          proofLoop ls ot options h fh
	       else do
	          putStrLn $ "Error: No such prover: "
	                     ++ cs
	          proofLoop ls ot options h fh
	 "l" : [] ->
	    getPath options
	 "l" : p: [] ->
	    openTheory options p Nothing
         _ -> do
	    (ls', ot', h', fh') <-
	       proveOne ls t ot options input h fh
            proofLoop ls' ot' options h' fh'
\end{code}

\begin{code}
proveOne :: SparseSet Literal -> Theory -> OTheory
   -> Options -> String  -> OHist -> FHist
   -> IO (SparseSet Literal, OTheory, OHist, FHist)
proveOne ls t ot options input h fh =
   case (checkParse lexerL (total taggedLiteralP)
         &? checkNoVars) input of
      CheckFail msg -> do
         putStrLn msg
	 return (ls, ot, h, fh)
      CheckPass tl -> do
         (ls', ot', h', fh', _) <-
	    oprove ls t ot options "nhlt" tl h fh
	 return (ls', ot', h', fh')
\end{code}

\begin{code}
showHelp :: IO ()
showHelp = putStrLn
   "To prove things: type a tagged literal.\n\
   \Other commands:\n\
   \   ?        = this message\n\
   \   q        = quit\n\
   \   t        = print theory\n\
   \   tp       = print theory in d-Prolog syntax\n\
   \   td       = print theory in delores syntax\n\
   \   f        = forget history\n\
   \   e        = show current prover engine\n\
   \   e prover = select prover engine from {-, n, nh,\
                  \ nhl, t, nt, nht, nhlt, nH, nHl}\n\
   \   l [path] = read a new theory file\
                  \ [named path]."
\end{code}

\begin{code}
quit :: IO ()
quit = putStrLn "Goodbye."
\end{code}
