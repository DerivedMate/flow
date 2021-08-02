module Eval.Step.Higher where

import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Data.Functor
import           Data.List
import           Debug.Pretty.Simple
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


stepHigher step (Cell MGen a     ) s  = pure [Datum (Anchor AGen a a) s]
stepHigher step (Anchor AGen e0 e) s0 = step e s0 >>= aux
 where
  aux :: [Datum] -> IO [Datum]
  aux ds = mapM discriminant ds <&> concat

  discriminant :: Datum -> IO [Datum]
  discriminant d | Datum Nil s' <- d = (pure . matchGenResult) s'
                 | Datum e' s' <- d  = pure [Datum (Anchor AGen e0 e') s']

  matchGenResult :: State -> [Datum]
  matchGenResult s
    | RTTuple [y, k, RTBool doEmit] <- l, doEmit
    = [ Datum Nil                 (s { stLast = y })
      , Datum (Anchor AGen e0 e0) (s0 { stLast = k })
      ]
    | RTTuple [y, k] <- l
    = matchGenResult $ s { stLast = RTTuple [y, k, RTBool True] }
    | RTTuple [y] <- l
    = matchGenResult $ s { stLast = RTTuple [y, y, RTBool True] }
    | RTNil <- l
    = []
    | r <- l
    = matchGenResult $ s { stLast = RTTuple [r, r, RTBool True] }
    where l = stLast s


stepHigher step (Cell MUnfold e0) s0 =
  stepHigher step (Anchor AUnfold e0 e0) s0
stepHigher step (Anchor AUnfold e0 e) s0 = manage [] [Datum e s0] <&> wrap
 where
  wrap :: [RTVal] -> [Datum]
  wrap vs = [Datum Nil s0 { stLast = RTList vs }]

  iter :: Datum -> IO [Datum]
  iter (Datum e s) = step e s

  manage :: [RTVal] -> [Datum] -> IO [RTVal]
  manage acc ds | null ds'  = (pure . reverse) (acc' <> acc)
                | otherwise = mapM iter ds' >>= manage (acc' <> acc) . concat
    where (acc', ds') = (bimap concat concat . unzip . fmap aggregate) ds

  -- Separate results from circulating data
  aggregate :: Datum -> ([RTVal], [Datum])
  aggregate d
    | isDone d, (y, k, True) <- l d  = ([y], [Datum e0 s0 { stLast = k }])
    | isDone d, (_, _, False) <- l d = ([], [])
    | otherwise                      = ([], [d])
   where
    isDone = (== Nil) . datumExp
    l      = matchResult . stLast . datumState

  -- Gen-style return correction
  matchResult :: RTVal -> (RTVal, RTVal, Bool)
  matchResult (RTTuple [y, k, doEmit]) =
    let RTBool doEmit' = cast TBool doEmit in (y, k, doEmit')
  matchResult (RTTuple [y, k]) = (y, k, True)
  matchResult RTNil            = (RTNil, RTNil, False)
  matchResult r                = (r, r, True)


stepHigher step (Cell MFold e0) s0 = stepHigher
  step
  (Anchor AFold e0 e0)
  (s0 { stLast = RTTuple [a0, xs] })
  where (a0 : xs : _) = deTuple (stLast s0)

stepHigher step (Anchor AFold e0 e) s
  |
  -- End fold
    e == Nil
  , (a, [_]) <- destState s
  = pure [Datum Nil s { stLast = a }]
  |

  -- End iteration. Begin new fold
    e == Nil
  = let (a, _ : xs) = destState s
    in  pure
          [Datum (Anchor AFold e0 e0) (s { stLast = RTTuple [a, RTList xs] })]
  |

  -- First step. Apply (acc; ...x)
    e == e0
  = let (a, x : _) = destState s
    in  step e (s { stLast = RTTuple (a : spreadX x) })
          >>= mapM (pure . reAnchor)
  |

  -- Evaluate e in the inner context, and keep track of the main list
    otherwise
  = let (a, _) = destState s
    in  step e (s { stLast = a }) >>= mapM (pure . reAnchor)
 where
  destState :: State -> (RTVal, [RTVal])
  destState s = let RTTuple [a, RTList xs] = stLast s in (a, xs)

  spreadX :: RTVal -> [RTVal]
  spreadX (RTTuple xs) = xs
  spreadX x            = [x]

  reAnchor :: Datum -> Datum
  reAnchor (Datum e' s') =
    let (_, xs) = destState s
    in  Datum (Anchor AFold e0 e')
              (s' { stLast = RTTuple [stLast s', RTList xs] })


stepHigher _ d _ = error $ "Unmatched expression in `stepHigher`: \n" <> show d
