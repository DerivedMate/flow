module Eval.Step.IO where
import           Eval.RT
import           Eval.Step.Common
import           Syntax

stepIO :: StepFunction
stepIO (Io (IoStdIn t)) s = do
    l <- getLine
    pure [Datum Nil (s { stLast = rtParse t l })]

stepIO (Io (IoStdOut t)) s = do
    case stLast s of
        (RTTuple (l : ls)) ->
            print (cast t l) >> pure [Datum Nil (s { stLast = RTTuple ls })]
        l -> print (cast t l) >> pure [Datum Nil (s { stLast = RTNil })]

stepIO d _ = error $ "Unmatched expression in `stepIO`: \n" <> show d
