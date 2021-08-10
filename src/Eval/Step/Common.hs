module Eval.Step.Common where
import           Data.Bifunctor
import           Data.Functor
import           Eval.RT
import           Syntax

type StepFunction = Exp -> State -> IO [Datum]
type IterFunction = Datum -> IO [Datum]

stepExhaust :: StepFunction -> StepFunction
stepExhaust step e s = exhaust step e s <&> (\l -> [Datum Nil s { stLast = l }])

exhaust :: StepFunction -> Exp -> State -> IO RTVal
exhaust step e s = step e s >>= exhaustEval []
 where
  exhaustEval :: [RTVal] -> [Datum] -> IO RTVal
  exhaustEval rs ds
    | null ds, [r] <- rs
    = pure r
    | null ds
    = pure (RTTuple rs)
    | otherwise
    = mapM iter ds >>= uncurry exhaustEval . bimap concat concat . unzip

  iter :: Datum -> IO ([RTVal], [Datum])
  iter (Datum Nil s) = pure ([stLast s], [])
  iter (Datum e s) =
    step e s <&> bimap concat concat . unzip . fmap interpretReturn

  interpretReturn :: Datum -> ([RTVal], [Datum])
  interpretReturn (Datum Nil s') = ([stLast s'], [])
  interpretReturn d              = ([], [d])
