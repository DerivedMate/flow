{-# LANGUAGE TupleSections #-}

module Eval where
import           Data.List
import           Data.Maybe
import           Debug.Pretty.Simple
import           Debug.Trace
import           Eval.RT
import           Eval.Step
import           Syntax


runFlow :: Maybe (Exp, String) -> IO ()
runFlow Nothing = error "Failed to compile the source"
runFlow (Just (_, r)) | (not . null) r = runFlow Nothing
runFlow (Just (ast, _)) = step ast (State RTNil []) >>= aux >> pure ()
 where
  aux [] = pure ()
  aux ds = sequence (iter <$> ds) >>= aux . concat
  iter (Datum e s) = step e s
  traceVars :: [String] -> Datum -> Datum
  traceVars ns d@(Datum e s) = traceShow vs d
    where vs = [ (n, getVar n s) | n <- ns ]
  traceLast :: Datum -> Datum
  traceLast d@(Datum e s) = traceShow (stLast s) d
  help ds = trace
    ("\n" <> intercalate " <<<<<<<< END >>>>>>>> \n\n" (fmap show ds) <> "\n\n")
    ds
