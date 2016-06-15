\module{DProver} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

See the user's guide (section~\ref{DProverUser}) for a
description of this module.

\begin{code}
module Main (main) where
\end{code}

\begin{code}
import System.Environment; import System.CPUTime; import Data.Char
import Data.List; import System.IO; import Control.Exception; import System.IO.Error
\end{code}

\begin{code}
import ABR.Parser; import ABR.Args; import ABR.List
import ABR.Text.String; import ABR.Control.Check
import ABR.Data.BSTree; import ABR.Parser.Checks
\end{code}

\begin{code}
import DefeasibleLexer; import Literal; import DTheory
import History; import DInference; import DProve
import DRunFile
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
   let (options,others) =
          findOpts [ParamS "e", FlagS "t", FlagS "tp",
             FlagS "td", ParamS "r"] (words args)
   case others of
       []   -> getPath options
       p:[] -> openTheory options p Nothing
       p:f  -> openTheory options p (Just (unwords f))
\end{code}

\begin{code}
getPath :: Options -> IO ()
getPath options = do
   putStr "Theory file name (or \"q\" to quit): "
   hFlush stdout
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
   source <- catch (readFile path) (\ (NoMethodError e) -> return "\0")
   case source of
      "\0" -> do
         putStrLn $ "Error: File " ++ path ++ " is \
            \empty or could not be read."
         getPath options
      _ -> case (checkParse lexerL (total theoryP)
            &? cyclesCheck  &? groundCheck) source of
         CheckFail msg -> do
            putStrLn msg
            case mtl of
               Nothing -> getPath options
               _       -> quit
         CheckPass t   -> do
            case (lookupBST "t" options,
	          lookupBST "tp" options,
	          lookupBST "td" options,
	          lookupBST "r" options) of
	       (Just FlagMinus,_,_,_) ->
	          putStr $ show t
	       (_,Just FlagMinus,_,_) ->
	          putStr $ show $ PrologTheory t
	       (_,_,Just FlagMinus,_) ->
	          putStr $ show $ DeloresTheory t
	       (_,_,_,Just (ParamValue rFile)) ->
	          doRunFile t options rFile
	       _ -> case mtl of
                  Nothing    ->
	             interactive t options
                  Just l -> do
	             proveOne t options l emptyHistory
	                 emptyHistory
	             return ()
\end{code}

\begin{code}
interactive :: Theory -> Options -> IO ()
interactive t options = do
      putStrLn "Type \"?\" for help."
      proofLoop options emptyHistory emptyHistory
   where
   proofLoop :: Options -> Hist -> WFHist -> IO ()
   proofLoop options h wh = do
      -- traceIO ("Options " ++ show options)
      -- traceIO ("h " ++ show h)
      -- traceIO ("wh " ++ show wh)
      putStr "|- "
      hFlush stdout
      input <- getLine
      let input' = words input
      case input' of
         [] ->
	    proofLoop options h wh
         "?" : _ -> do
	    showHelp
	    proofLoop options h wh
         "q" : _ ->
	    quit
         "t" : _ -> do
	    putStrLn $ show t
            proofLoop options h wh
         "tp": _ -> do
	    putStrLn $ show $ PrologTheory t
            proofLoop options h wh
         "td" : _ -> do
	    putStrLn $ show $ DeloresTheory t
            proofLoop options h wh
         "f" : _ -> do
	    putStrLn "Those who forget history \
	             \are destined to repeat it."
            proofLoop options emptyHistory emptyHistory
	 "e" : css ->
	    let cs = unwords css
	    in if cs `elem` ["-","n","nt","nh","nht",
	          "nhl","nhlt","nhlw","nhlwt"] then
	          proofLoop (updateBST (\x _ -> trace ("In DProver line 138 in interactive param x= " ++ show x) x)
	          "e" (ParamValue cs) options) h wh
	       else if cs == "" then do
	          putStr "Current prover: "
	          case lookupBST "e" options of
	             Nothing ->
	                putStrLn $ "nhlt"
	             Just (ParamValue p) ->
	                putStrLn p
	          proofLoop options h wh
	       else do
	          putStrLn $ "Error: No such prover: "
	                     ++ cs
	          proofLoop options h wh
         "r" : rFile : _ -> do
            doRunFile t options rFile
            proofLoop options h wh
	 "l" : [] ->
	    getPath options
	 "l" : p: [] ->
	    openTheory options p Nothing
         _ -> do
	    (h',wh') <- proveOne t options input h wh
            proofLoop options h' wh'
\end{code}

\begin{code}
showHelp :: IO ()
showHelp = putStrLn
   "To prove things: type a tagged literal.\n\
   \Other commands:\n\
   \   ?          = this message\n\
   \   q          = quit\n\
   \   t          = print theory\n\
   \   tp         = print theory in d-Prolog syntax\n\
   \   td         = print theory in Delores syntax\n\
   \   f          = forget history\n\
   \   e          = show current prover engine\n\
   \   e prover   = select prover engine from {-, n, nh, \
                    \ nhl, nhlw, t, nt, nht, nhlt, nhlwt}\
   \   r run-file = run the tests in run-file\n\
   \   l [path]   = read a new theory file\
                    \ [named path].\n"
\end{code}

\begin{code}
proveOne :: Theory -> Options -> String  -> Hist -> WFHist
   -> IO (Hist,WFHist)
proveOne t options input h wh
   = case (checkParse lexerL (total taggedLiteralP)
           &? checkNoVars) input of
        CheckFail msg -> do
	   putStrLn msg
	   return (h,wh)
        CheckPass tl -> do
	   (h',wh',_) <- prove t options "nhlt" tl h wh

	   return (h', wh')

\end{code}

\begin{code}
quit :: IO ()
quit = putStrLn "Goodbye."
\end{code}

\begin{code}
doRunFile :: Theory -> Options -> FilePath -> IO ()
doRunFile t@(Theory fs rs ps) options rFile = do
   source <- catch (readFile rFile) (\(NoMethodError e) -> return "\0")
   print rFile
   case source of
      "\0" -> putStrLn $ "Can't read file: " ++ rFile
      _    -> case checkParse lexerL runFileP source of
         CheckFail msg     -> do
            putStrLn msg
         CheckPass runFile@(_,_,tls) -> do
            let runs :: [Run]
                runs = generateRuns runFile
                run1 :: Run -> Tagged Literal -> IO String
                run1 fs' tl = do
                   (_,_,r) <- prove (Theory (fs' ++ fs)
                      rs ps) options "nhlt" tl
                      emptyHistory emptyHistory

                   case r of
                      "Proved"     -> return "P"
                      "Not proved" -> return "N"
                      "Loops"      -> return "L"
                runRun :: Run -> IO [String]
                runRun run = mapM (run1 run) tls
            rss <- mapM runRun runs
            putStrLn "\nSummary table:"
            let table = if null runs
                   then [["No runs."]]
                   else (map (const "") (head runs) ++
                        map show tls) :
                        zipWith (\run rs ->
                           map show run ++ rs) runs rss
                widths = map ((+2) . maximum) $
                   (map . map) length $
                   transpose table
                spaceOut :: [Int] -> [String] -> String
                spaceOut ws css = concat $
                   zipWith (\w cs -> rJustify w cs) ws css
                table' = map (spaceOut widths) table
            putStr $ unlines $ table'
\end{code}
