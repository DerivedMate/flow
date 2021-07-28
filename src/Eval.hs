{-# LANGUAGE TupleSections #-}

module Eval where
import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Debug.Pretty.Simple
import           Debug.Trace
import           Eval.RT
import           Eval.Step.Common
import           Eval.Step.Control
import           Eval.Step.Func
import           Eval.Step.Higher
import           Eval.Step.IO
import           Eval.Step.Literal
import           GHC.Stack
import           Helper.AssocMap
import           Syntax
import           System.IO
import           System.IO.Unsafe
import           Text.Pretty.Simple
import           Text.Show

{--------------------------------:
    Main Line
:--------------------------------}

step :: StepFunction
step Nil                 _ = pure []


{--------------------------------:
    Literal 
:--------------------------------}

step d@(LInt    _      ) s = stepLiteral step d s
step d@(LFloat  _      ) s = stepLiteral step d s
step d@(LBool   _      ) s = stepLiteral step d s
step d@(LString _      ) s = stepLiteral step d s

step d@(LTuple  _      ) s = stepLiteral step d s
step d@(LList   _      ) s = stepLiteral step d s
step d@(BinOp _ _ _    ) s = stepLiteral step d s


{--------------------------------:
    Higher
:--------------------------------}

step d@(Cell _ a       ) s = stepHigher step d s
step d@(Anchor MGen _ _) s = stepHigher step d s


{--------------------------------:
    Functional
:--------------------------------}

step d@(Func   _    _ _) s = stepFunc step d s
step d@(FRef _         ) s = stepFunc step d s
step d@(Var  _         ) s = stepFunc step d s


{--------------------------------:
    Control    
:--------------------------------}

step d@(Flow    _ _    ) s = stepControl step d s
step d@(Program _ _    ) s = stepControl step d s


{--------------------------------:
    IO
:--------------------------------}

step e@(Io _           ) s = stepIO e s

step _                   _ = undefined

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
