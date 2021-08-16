module Eval.Step.Func where

import           Data.Functor
import           Data.List
import           Data.Maybe
import           Debug.Pretty.Simple
import           Eval.RT
import           Eval.Step.Common
import           Syntax

stepFunc :: StepFunction -> StepFunction
stepFunc step node@(Func l args rt f_body) s =
  let
      -- Define the function if is labeled
    fns = maybe
      []
      (\k ->
        [ (k, RTFunc l (rtArgOfArg <$> args) TAny f_body)
        | not (funcExists k s)
        ]
      )
      l

    -- Check for partial application
    (last, vars, doRun) = if length args > rtLength (stLast s)
      then
        let (appliedArgs, leftArgs) = splitAt (rtLength (stLast s)) args
        in  ( RTFunc
              l
              (  bindArgs (stLast s) appliedArgs
              <> [ RTArg n t RTNil | Arg n t <- leftArgs ]
              )
              TAny
              f_body
            , []
            , False
            )
      else (RTNil, assignVars (stLast s) args <> fns, True)
    f' = vars
    s' = State { stLast = last, stStack = f' : stStack s }
  in
    if doRun then stepFExp f_body s' else pure [Datum Nil s']
 where
  bindArgs :: RTVal -> [Arg] -> [RTArg]
  bindArgs l args =
    let tArgs = map (\(Arg n t) -> (n, t)) args
    in  [ RTArg n t v | ((n, t), v) <- tArgs `zip` deTuple l ]
  stepFExp :: FuncExp -> State -> IO [Datum]
  stepFExp FNil s = pure [Datum Nil (s { stStack = tail . stStack $ s })]
  stepFExp (Single a) s = step (Anchor AClosure node a) s
  stepFExp (Cond c a b) s = exhaust step c s >>= matchResult . cast TBool
   where
    matchResult (RTBool True ) = stepFExp (Single a) s
    matchResult (RTBool False) = stepFExp b s
    matchResult l              = error
      (  "Undefined boolean cast in function conditional:"
      <> "\n[Cast]: "
      <> show l
      <> "\n[State]: "
      <> show s
      <> "\n[Condition]: "
      <> show c
      )

stepFunc step (FRef k) s = if funcExists k s
  then step f s'
  else error ("Undefined function '" <> k <> "'. State:\n" <> show s)
 where
  splatLast :: RTVal -> [RTVal]
  splatLast (RTTuple ls) = ls
  splatLast r            = [r]
  Just (RTFunc l args rt fe) = getVar k s
  s'                         = s
    { stLast = RTTuple
               $  [ v | RTArg _ _ v <- args, RTNil /= v ]
               <> (splatLast . stLast) s
    }
  f = Func l (argOfRtArg <$> args) rt fe

stepFunc _ (Var k) s | Just RTFunc{} <- v = pure [Datum (FRef k) s]
                     | Just vv <- v = pure [Datum Nil s { stLast = vv }]
                     | otherwise = error ("variable '" <> k <> "' not found")
  where v = getVar k s

stepFunc step (Capture c) s
  | Right l'' <- l'     = pure [Datum Nil s { stLast = l'' }]
  | Left errorMsg <- l' = error errorMsg
  where l' = getCapture c s

stepFunc _ d _ = error $ "Unmatched expression in `stepFunc`: \n" <> show d
