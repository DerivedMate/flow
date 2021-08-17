module Helper.Debug where

import Eval.RT
import Debug.Trace

traceVars :: [String] -> Datum -> Datum
traceVars ns d@(Datum e s) = traceShow vs d
  where vs = [ (n, getVar n s) | n <- ns ]
traceLast :: Datum -> Datum
traceLast d@(Datum e s) = traceShow (stLast s) d