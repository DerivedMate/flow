module Eval.Step where

import           Eval.Step.Common
import           Eval.Step.Control
import           Eval.Step.Func
import           Eval.Step.Higher
import           Eval.Step.IO
import           Eval.Step.Literal
import           Syntax

step :: StepFunction
step Nil              _ = pure []


{--------------------------------:
    Literal 
:--------------------------------}

step d@(LInt    _   ) s = stepLiteral step d s
step d@(LFloat  _   ) s = stepLiteral step d s
step d@(LBool   _   ) s = stepLiteral step d s
step d@(LString _   ) s = stepLiteral step d s

step d@(LTuple  _   ) s = stepLiteral step d s
step d@(LList   _   ) s = stepLiteral step d s
step d@(BinOp _ _ _ ) s = stepLiteral step d s


{--------------------------------:
    Higher
:--------------------------------}

step d@(Cell _ _    ) s = stepHigher step d s
step d@(Anchor _ _ _) s = stepHigher step d s


{--------------------------------:
    Functional
:--------------------------------}

step d@(Func   _ _ _) s = stepFunc step d s
step d@(FRef _      ) s = stepFunc step d s
step d@(Var  _      ) s = stepFunc step d s
step d@(Capture _ _ ) s = stepFunc step d s


{--------------------------------:
    Control    
:--------------------------------}

step d@(Flow    _ _ ) s = stepControl step d s
step d@(Program _ _ ) s = stepControl step d s


{--------------------------------:
    IO
:--------------------------------}

step e@(Io _        ) s = stepIO e s
step _                _ = undefined
