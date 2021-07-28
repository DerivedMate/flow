{-# LANGUAGE TupleSections #-}

module Eval where
import           Data.List
import           Debug.Pretty.Simple
import           Debug.Trace
import           Eval.RT
import           Eval.Step
import           Syntax


runFlow :: Maybe (Exp, a0) -> IO ()
runFlow Nothing         = error "Filed to compile the source"
runFlow (Just (ast, _)) = step ast (State RTNil []) >>= aux >> pure ()
  where
    aux [] = pure ()
    aux ds = sequence (iter <$> ds) >>= aux . concat
    iter (Datum e s) = step e s
    help ds = trace
        (  "\n"
        <> intercalate " <<<<<<<< END >>>>>>>> \n\n" (fmap show ds)
        <> "\n\n"
        )
        ds
