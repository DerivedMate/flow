module Eval.Step.Func where

import           Data.Functor
import           Data.List
import           Data.Maybe
import           Eval.RT
import           Eval.Step.Common
import           Syntax

stepFunc :: StepFunction -> StepFunction
stepFunc step node@(Func l args f_body) s =
  let
      -- Define the function if is labeled
      fns = maybe [] (\k -> [ (k, RTFunc node) | not (funcExists k s) ]) l

      -- Check for partial application
      (last, vars, doRun) = if length args > rtLength (stLast s)
        then
          let (appliedArgs, leftArgs) = splitAt (rtLength (stLast s)) args
          in  ( RTFunc (Func l leftArgs f_body)
              , assignVars (stLast s) appliedArgs
              , False
              )
        else (RTNil, assignVars (stLast s) args, True)
      f' = vars `union` fns
      s' = State { stLast = last, stStack = f' : stStack s }
  in  if doRun then stepFExp f_body s' <&> fmap fixLast else pure [Datum Nil s']
 where
  stepFExp :: FuncExp -> State -> IO [Datum]
  stepFExp FNil         s = pure [Datum Nil s]
  stepFExp (Single a  ) s = step a s
  stepFExp (Cond c a b) s = step c s >>= mapM matchResult <&> concat
   where
    matchResult r
      | Datum Nil s' <- r = case (cast TBool . stLast) s' of
        RTBool True  -> step a s
        RTBool False -> stepFExp b s
        r            -> error
          (  "Undefined boolean cast in func conditional:"
          <> "\n[Cast]: "
          <> show r
          <> "\n[State]: "
          <> show s
          <> "\n[Condition]: "
          <> show c
          )
      | Datum c' s' <- r = stepFExp (Cond c' a b) s'

  fixLast :: Datum -> Datum
  fixLast d | datumExp d == Nil = head $ correctLast args [d]
            | otherwise         = d

stepFunc step (FRef k) s = do
  if funcExists k s
    then step f s
    else error ("Undefined function '" <> k <> "'. State:\n" <> show s)
  where f = funcFind k s

stepFunc _ (Var k) s | Just (RTFunc f) <- v = pure [Datum f s]
                     | Just vv <- v = pure [Datum Nil s { stLast = vv }]
                     | otherwise = error ("variable '" <> k <> "' not found")
  where v = getVar k s

stepFunc step (Capture c) s
  | Right l'' <- l'     = pure [Datum Nil s { stLast = l'' }]
  | Left errorMsg <- l' = error errorMsg
  where l' = getCapture c s

stepFunc _ d _ = error $ "Unmatched expression in `stepFunc`: \n" <> show d
