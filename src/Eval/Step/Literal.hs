module Eval.Step.Literal where

import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Data.Functor
import           Debug.Pretty.Simple            ( pTraceShowId )
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
  va <- exhaust step a s
  vb <- exhaust step b s
  pure [Datum Nil (s { stLast = fOfBop op va vb })]

stepLiteral step (Casting e t) s = step e s <&> map matchResult
 where
  matchResult (Datum e s')
    | Nil <- e  = Datum e (s' { stLast = cast t (stLast s') })
    | otherwise = Datum (Casting e t) s'

stepLiteral _ d _ =
  error $ "Unmatched expression in `stepLiteral`: \n" <> show d
