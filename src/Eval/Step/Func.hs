module Eval.Step.Func where

import           Data.Functor
import           Data.List
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
                let (appliedArgs, leftArgs) =
                        splitAt (rtLength (stLast s)) args
                in  ( RTFunc (Func l leftArgs f_body)
                    , assignVars (stLast s) appliedArgs
                    , False
                    )
            else (RTNil, assignVars (stLast s) args, True)
        f' = vars `union` fns
        s' = State { stLast = last, stStack = f' : stStack s }
    in
        if doRun
            then stepFExp f_body s' <&> fmap fixLast
            else pure [Datum Nil s']
  where
    stepFExp :: FuncExp -> State -> IO [Datum]
    stepFExp FNil         s = pure [Datum Nil s]
    stepFExp (Single a  ) s = step a s
    stepFExp (Cond c a b) s = do
        (rc : _) <- step c s
        case (cast TBool . stLast . datumState) rc of
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
    fixLast :: Datum -> Datum
    fixLast d | datumExp d == Nil = head $ correctLast args [d]
              | otherwise         = d

stepFunc step (FRef k) s = do
    if funcExists k s
        then step f s
        else error ("Undefined function '" <> k <> "'. State:\n" <> show s)
    where f = funcFind k s

stepFunc _ (Var k) s | RTFunc f <- v = pure [Datum f s]
                     | otherwise     = pure [Datum Nil s { stLast = v }]
  where
    v = findVar (concat $ stStack s)
    findVar [] = RTNil
    findVar ((k', v) : ds) | k == k'   = v
                           | otherwise = findVar ds

stepFunc _ d _ = error $ "Unmatched expression in `stepFunc`: \n" <> show d
