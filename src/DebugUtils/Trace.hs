module DebugUtils.Trace
    (
    trace
    ) where

import qualified Debug.Trace as D

trace :: String -> a -> a
trace xs = D.trace ("+++++ " ++  xs)
