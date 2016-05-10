\module{DTScale} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

See the user's guide (section~\ref{DTScaleUser}) for a
description of this module.

\begin{code}
module Main (main) where
\end{code}

\begin{code}
import System.Environment; import System.CPUTime
\end{code}

\begin{code}
import ABR.Args; import ABR.SparseSet
import ABR.Data.BSTree
\end{code}

\begin{code}
import Literal; import DRule; import DTheory
import History; import DTestTheories; import DInference
import DProve; import ODTheory
\end{code}

\begin{code}
main :: IO ()
main = do
   args <- getArgs
   run' args
\end{code}

\begin{code}
run :: String -> IO () -- Hugs entry point
run = run' . words
\end{code}

\begin{code}
run' :: [String] -> IO ()
run' args =
   let (options,thName:sizes) = findOpts [ParamS "e",
          FlagS "t", FlagS "tp", FlagS "td", FlagS "o",
	  FlagS "m"]
	  args
       sizes' = map read sizes
       th = generateTheory thName sizes'
       tl = generateTL thName sizes'
   in case (th, tl) of
      (Nothing, _) ->
         putStrLn ("ERROR: no such theory: " ++
	    thName ++ " " ++ unwords sizes)
      (_, Nothing) ->
         putStrLn ("ERROR: no such tagged literal: " ++
	    thName ++ " " ++ unwords sizes)
      (Just th, Just tl) -> case (lookupBST "t" options,
         lookupBST "tp" options, lookupBST "td" options) of
	 (Just FlagMinus,_,_) ->
            putStr $ show th
	 (_,Just FlagMinus,_) ->
            putStr $ show $ PrologTheory th
	 (_,_,Just FlagMinus) ->
            putStr $ show $ DeloresTheory th
	 _ -> do
	    case lookupBST "m" options of
	       Just FlagMinus -> do
	          let Just (f,r,p,s) =
	                 generateMetrics thName sizes'
		  putStrLn $
		     "Computed metrics:"
		     ++ "\n   # facts =      " ++ show f
		     ++ "\n   # rules =      " ++ show r
		     ++ "\n   # priorities = " ++ show p
		     ++ "\n   size =         " ++ show s
		     ++ "\n"
	       _ -> return ()
	    let Theory fs rs ps = th
	        nfs = length fs
		nrs = length rs
		nps = length ps
		nls = sum $ map (length . antecedent) rs
            putStrLn $ "\n\n# facts:      " ++ show nfs
            putStrLn $ "# rules:      " ++ show nrs
            putStrLn $ "# priorities: " ++ show nps
            putStrLn $ "# literals in all bodies: "
                       ++ show nls
	    putStrLn $ "### total size = "
	               ++ show (nfs + nrs + nps + nls)
		       ++ "\n"
	    case lookupBST "o" options of
	       Just FlagMinus -> do
	          _ <- prove th options "nhl" tl
		          emptyHistory emptyHistory
	          return ()
	       _ -> do
	          let ls = getLits th emptySS
		      ot = makeOTheory ls th
		      fh = initPmSyLitHist ot
		      dummy = Plus PS_D (PosLit "bogus")
		  putStrLn $ show $ length $ show ot
		  putStrLn "Dummy Run"
		  (ls', ot', h', fh', _) <-
		     oprove ls th ot options "nHl"
		        dummy emptyHistory fh
		  putStrLn "Real Run"
		  _ <- oprove ls th ot options "nHl"
		          tl h' fh'
		  return ()
\end{code}
