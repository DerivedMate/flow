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
  va <- step a s >>= exhaustEval []
  vb <- step b s >>= exhaustEval []
  pure [Datum Nil (s { stLast = fOfBop op va vb })]

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


stepLiteral _ d _ =
  error $ "Unmatched expression in `stepLiteral`: \n" <> show d
