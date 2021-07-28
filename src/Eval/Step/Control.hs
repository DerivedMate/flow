module Eval.Step.Control where

import           Eval.RT
import           Eval.Step.Common
import           Syntax

stepControl :: StepFunction -> StepFunction
stepControl step self@(Flow a b) s = map aux <$> step a s
   where
      aux r | Datum Nil s' <- r = Datum b s'
            | Datum a' s' <- r  = Datum (Flow a' b) s'

stepControl step self@(Program a b) s = map aux <$> step a s
   where
      aux r | Datum Nil s' <- r = Datum b (s' { stLast = RTNil })
            | Datum a' s' <- r  = Datum (Program a' b) s'

stepControl _ d _ =
      error $ "Unmatched expression in `stepControl`: \n" <> show d
