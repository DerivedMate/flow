module Eval.Step.Common where
import           Eval.RT
import           Syntax

type StepFunction = Exp -> State -> IO [Datum]
