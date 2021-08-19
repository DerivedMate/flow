module Eval.Step.Func where

import           Control.Monad
import           Control.Monad                  ( join )
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Debug.Pretty.Simple
import           Eval.RT
import           Eval.Step.Common
import           Helper.MaybeT
import           Syntax

stepFunc :: StepFunction -> StepFunction
stepFunc step node@(Func l args rt f_body) s =
  let
      -- Define the function if is labeled
    fns = maybe
      []
      (\k ->
        [ (k, RTFunc l (rtArgOfArg <$> args) rt f_body) | not (funcExists k s) ]
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
              rt
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
  | CSingle e <- c = exhaust step e s >>= matchResult . auxSingle
  | CSlice ei mej <- c = do
    i <- exhaustE ei
    j <- unwrapMaybeT $ MaybeT (pure mej) >>= MaybeT . fmap Just . exhaustE
    matchResult $ auxSlice i j
 where
  exhaustE e = exhaust step e s
  auxSingle :: RTVal -> Either String RTVal
  auxSingle v
    | RTInt i <- cast TInt v = getCaptureSingle i s
    | otherwise =  Left
    $  "error casting capture index to Int; index = "
    <> show v

  auxSlice :: RTVal -> Maybe RTVal -> Either String RTVal
  auxSlice vi vj
    | RTInt i <- ci
    , Just (RTInt j) <- mcj
    = getCaptureSlice i (Just j) s
    | RTInt i <- ci
    , Nothing <- mcj
    = getCaptureSlice i Nothing s
    | otherwise
    = Left
      $  "error casting one/both indices of capture slice; indices = ("
      <> show ci
      <> ", "
      <> show mcj
      <> ")"
   where
    ci  = cast TInt vi
    mcj = fmap (cast TInt) vj

  matchResult :: Either String RTVal -> IO [Datum]
  matchResult l' | Right l'' <- l'     = pure [Datum Nil s { stLast = l'' }]
                 | Left errorMsg <- l' = error errorMsg

stepFunc _ d _ = error $ "Unmatched expression in `stepFunc`: \n" <> show d
