module Eval.Step.Higher where

import           Data.Functor
import           Eval.RT
import           Eval.Step.Common
import           Syntax

stepHigher :: StepFunction -> StepFunction
stepHigher step (Cell MNone a) s = step a s
stepHigher step (Cell MMap e) s0 =
  mapM (runner []) [ [Datum e (s0 { stLast = a })] | a <- as0 ] >>= rejoin
 where
  as0 = (deTuple . stLast) s0
  iter (Datum e s) = step e s
  rejoin ls = pure
    [ Datum
        Nil
        s0 { stLast = wrapperOfState (stLast s0) (concat (reverse <$> ls)) }
    ]

  runner :: [RTVal] -> [Datum] -> IO [RTVal]
  runner ready [] = pure ready
  runner ready circ =
    let (rs, cs) = retire circ ready [] in mapM iter cs >>= runner rs . concat

  retire :: [Datum] -> [RTVal] -> [Datum] -> ([RTVal], [Datum])
  retire [] rs bs = (rs, bs)
  retire (c : cs) rs bs | Datum Nil s <- c = retire cs (stLast s : rs) bs
                        | otherwise        = retire cs rs (c : bs)


stepHigher step (Cell MKeep e) s = step e s >>= mapM aux <&> concat
 where
  discriminant r | RTBool d <- cast TBool r = d
                 | otherwise = error "Non-bool cast in keep predicate"

  aux (Datum Nil s') | discriminant (stLast s') = pure [Datum Nil s]
                     | otherwise                = pure []
  aux (Datum e' s') = pure [Datum (Cell MKeep e') s']


stepHigher step (Cell MKeepEnum e) s =
  mapM runBranch ((deTuple . stLast) s) <&> rewrap . foldl1 (<>)
 where
  runBranch :: RTVal -> IO [RTVal]
  runBranch v = exhaustBranch v [] [Datum e (s { stLast = v })]

  retire :: [Datum] -> ([RTVal], [Datum]) -> ([RTVal], [Datum])
  retire [] (rs, ys) = (rs, ys)
  retire (d : ds) (rs, ys)
    | Nil == datumExp d, RTBool True == (stLast . datumState) d = retire
      ds
      ((stLast . datumState) d : rs, ys)
    | Nil == datumExp d = retire ds (rs, ys)
    | otherwise = retire ds (rs, d : ys)

  exhaustBranch :: RTVal -> [RTVal] -> [Datum] -> IO [RTVal]
  exhaustBranch v retired [] =
    pure [ v | r <- retired, cast TBool r == RTBool True ]
  exhaustBranch v retired ds =
    let (retired', ds') = retire ds (retired, [])
    in  iter ds' >>= exhaustBranch v retired'

  iter :: [Datum] -> IO [Datum]
  iter ds = concat <$> mapM (\(Datum e s) -> step e s) ds

  rewrap :: [RTVal] -> [Datum]
  rewrap vs = [Datum Nil (s { stLast = (wrapperOfState . stLast) s vs })]


stepHigher step (Cell MGen a     ) s  = pure [Datum (Anchor MGen a a) s]
stepHigher step (Anchor MGen e0 e) s0 = step e s0 >>= aux
 where
  aux :: [Datum] -> IO [Datum]
  aux ds = mapM discriminant ds <&> concat

  discriminant :: Datum -> IO [Datum]
  discriminant d | Datum Nil s' <- d = (pure . matchResult) s'
                 | Datum e' s' <- d  = pure [Datum (Anchor MGen e0 e') s']

  matchResult :: State -> [Datum]
  matchResult s
    | RTTuple [y, k, RTBool doEmit] <- l, doEmit
    = [ Datum Nil                 (s { stLast = y })
      , Datum (Anchor MGen e0 e0) (s0 { stLast = k })
      ]
    | RTTuple [y, k] <- l
    = matchResult $ s { stLast = RTTuple [y, k, RTBool True] }
    | RTTuple [y] <- l
    = matchResult $ s { stLast = RTTuple [y, y, RTBool True] }
    | RTNil <- l
    = []
    | r <- l
    = matchResult $ s { stLast = RTTuple [r, r, RTBool True] }
    where l = stLast s


stepHigher _ d _ = error $ "Unmatched expression in `stepHigher`: \n" <> show d
