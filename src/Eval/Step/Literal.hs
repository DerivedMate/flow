module Eval.Step.Literal where

import           Eval.RT
import           Eval.Step.Common
import           Syntax

stepLiteral :: StepFunction -> StepFunction
stepLiteral _    (LInt    a ) s = pure [Datum Nil (s { stLast = RTInt a })]
stepLiteral _    (LFloat  a ) s = pure [Datum Nil (s { stLast = RTFloat a })]
stepLiteral _    (LBool   a ) s = pure [Datum Nil (s { stLast = RTBool a })]
stepLiteral _    (LString a ) s = pure [Datum Nil (s { stLast = RTString a })]

stepLiteral step (LTuple  ts) s = (: []) . aux . concat <$> mapM (`step` s) ts
 where
  aux ds =
    let l' = map (stLast . datumState) ds
    in  Datum Nil (s { stLast = RTTuple l' })

stepLiteral step (LList ls) s = step (LTuple ls) s >>= toTuple
 where
  toTuple :: [Datum] -> IO [Datum]
  toTuple [] = pure []
  toTuple (Datum _ s : _) =
    pure [Datum Nil s { stLast = RTList (deTuple $ stLast s) }]

stepLiteral step (BinOp op a b) s = do
  ra <- step a s
  case ra of
    (Datum Nil s' : _) -> do
      rb <- step b s'
      case rb of
        (Datum Nil s'' : _) ->
          pure [Datum Nil (s'' { stLast = fOfBop op (stLast s') (stLast s'') })]
        _ -> pure [Datum (BinOp op a b) s]
    _ -> pure [Datum (BinOp op a b) s]

stepLiteral _ d _ =
  error $ "Unmatched expression in `stepLiteral`: \n" <> show d
