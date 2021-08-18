{-# LANGUAGE TupleSections #-}

module Eval where
import           Data.List
import           Data.Maybe
import           Debug.Pretty.Simple
import           Debug.Trace
import           Eval.RT
import           Eval.Step
import           Lexer
import           Syntax
import           Text.Pretty.Simple


runFlow :: PReturn Exp -> IO ()
runFlow prs
  | Just pr <- r
  = step (prResult pr) (State RTNil []) >>= runLoop
  | Just pe <- e
  = pPrint pe
  where (e, r) = flDistillReturn prs

runFlow _ = undefined

runLoop :: [Datum] -> IO ()
runLoop [] = pure ()
runLoop ds = sequence (iter <$> ds) >>= runLoop . concat
iter :: Datum -> IO [Datum]
iter (Datum e s) = step e s
help :: Show a => [a] -> [a]
help ds = trace
  ("\n" <> intercalate " <<<<<<<< END >>>>>>>> \n\n" (fmap show ds) <> "\n\n")
  ds

