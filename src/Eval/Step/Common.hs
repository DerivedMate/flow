module Eval.Step.Common where
import           Eval.RT
import           Syntax

type StepFunction = Exp -> State -> IO [Datum]
type IterFunction = Datum -> IO [Datum]