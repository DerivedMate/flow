module Eval.Step where

import           Eval.Step.Common
import           Eval.Step.Control
import           Eval.Step.Func
import           Eval.Step.Higher
import           Eval.Step.IO
import           Eval.Step.Literal
import           Syntax

step :: StepFunction
step Nil         _ = pure []


{--------------------------------:
    Literal 
:--------------------------------}

step d@LInt{}    s = stepLiteral step d s
step d@LFloat{}  s = stepLiteral step d s
step d@LBool{}   s = stepLiteral step d s
step d@LString{} s = stepLiteral step d s

step d@LTuple{}  s = stepLiteral step d s
step d@LList{}   s = stepLiteral step d s
step d@BinOp{}   s = stepLiteral step d s
step d@Casting{} s = stepLiteral step d s


{--------------------------------:
    Higher
:--------------------------------}

step d@Cell{}    s = stepHigher step d s
step d@Anchor{}  s = stepHigher step d s


{--------------------------------:
    Functional
:--------------------------------}

step d@Func{}    s = stepFunc step d s
step d@FRef{}    s = stepFunc step d s
step d@Var{}     s = stepFunc step d s
step d@Capture{} s = stepFunc step d s


{--------------------------------:
    Control    
:--------------------------------}

step d@Flow{}    s = stepControl step d s
step d@Program{} s = stepControl step d s


{--------------------------------:
    IO
:--------------------------------}

step e@Io{}      s = stepIO e s
step _           _ = undefined
