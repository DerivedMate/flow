{-# LANGUAGE TupleSections #-}

module Reduction.Static where
import           Control.Applicative
import           Data.Bifunctor
import           Data.Function
import           Data.List
import           Data.Maybe
import           Debug.Pretty.Simple
import           Eval
import           Eval.RT
import           Reduction.Reducer
import           Syntax                  hiding ( bool )
import           Text.Pretty.Simple             ( pPrint )

rTest :: Rd FuncExp Bool
rTest = runReducer rStaticFuncExp
                   (emptyState { stLast = last, stStack = [vars] })
                   e
 where
  last = RTNil
  vars = [("a", RTInt 3)]
  -- [3; a=5]{ + &0 a } ~ { 8 }
  e    = Single (LTuple [BinOp OpAdd (LInt 3) (Var "a")])



rStaticFuncExp :: Reducer FuncExp Bool
rStaticFuncExp = Reducer aux
 where
  aux :: State -> FuncExp -> Rd FuncExp Bool
  aux s e
    | Single ee <- e
    = let Rd wasReduced s' ee' = runReducer rStaticExp s ee
      in  Rd wasReduced s (Single ee')
    | Cond c ee next <- e
    = let Rd cRed    _ c'    = runReducer rStaticExp s c
          Rd eeRed   _ ee'   = runReducer rStaticExp s ee
          Rd nextRed _ next' = aux s next
      in  Rd (cRed && eeRed && nextRed) s (Cond c' ee' next')
    | FNil <- e
    = Rd True s e

rStaticExp :: Reducer Exp Bool
rStaticExp = Reducer aux
 where
  evalLiteral :: State -> Exp -> Maybe RTVal
  evalLiteral s e
    | LBool a <- e      = Just $ RTBool a
    | LFloat a <- e     = Just $ RTFloat a
    | LInt a <- e       = Just $ RTInt a
    | LString a <- e    = Just $ RTString a
    | LList ls <- e     = RTList <$> mapM (evalLiteral s) ls
    | LTuple ls <- e    = RTTuple <$> mapM (evalLiteral s) ls
    | BinOp op p q <- e = fOfBop op <$> evalLiteral s p <*> evalLiteral s q
    | otherwise         = Nothing

  setCapture :: State -> Exp -> Either String RTVal -> Rd Exp Bool
  setCapture s _ (Right l') = Rd True (s { stLast = l' }) (expOfRt l')
  setCapture s e l          = Rd False s e

  aux :: State -> Exp -> Rd Exp Bool
  aux s e
    | LBool a <- e
    = Rd True (s { stLast = RTBool a }) e
    | LFloat a <- e
    = Rd True (s { stLast = RTFloat a }) e
    | LInt a <- e
    = Rd True (s { stLast = RTInt a }) e
    | LString a <- e
    = Rd True (s { stLast = RTString a }) e
    | Nil <- e
    = Rd True s { stLast = RTNil } e
    | LTuple ls <- e
    = let is  = all (rdVal . aux s) ls
          l'  = mapM (evalLiteral s . rdExp . aux s) ls
          l'' = fromMaybe [] l'
      in  if is && isJust l'
            then Rd is (s { stLast = RTTuple l'' }) ((LTuple . map expOfRt) l'')
            else Rd
              False
              s
              (LTuple $ map
                (\l -> maybe l expOfRt (evalLiteral s . rdExp $ aux s l))
                ls
              )
    | LList ls <- e
    = let Rd is s' (LTuple e') = aux s (LTuple ls)
          ls'                  = (deTuple . stLast) s'
      in  if is
            then Rd is (s { stLast = RTList ls' }) ((LList . map expOfRt) ls')
            else Rd False s (LList e')
    | Var v <- e
    = let v'  = getVar v s
          v'' = fromMaybe RTNil v'
      in  if isJust v'
            then Rd True s { stLast = v'' } (expOfRt v'')
            else Rd False s e
    | BinOp op p q <- e
    = let Rd staticP sp p' = aux s p
          Rd staticQ sq q' = aux s q
          l'               = (fOfBop op `on` stLast) sp sq
      in  if staticP && staticQ
            then Rd True s { stLast = l' } (expOfRt l')
            else Rd False s (BinOp op p' q')
    | Cell MNone e' <- e
    = let Rd v s' e'' = aux s e' in Rd v s' (Cell MNone e'')
    | Capture c <- e
    , RTNil <- stLast s
    = Rd False s e
    | Capture (CSingle ee) <- e
    , Just (RTInt i) <- cast TInt <$> evalLiteral s ee
    = setCapture s e (getCaptureSingle i s)
    | Capture (CSlice ei mej) <- e
    , Just (RTInt i) <- cast TInt <$> evalLiteral s ei
    , Just (RTInt j) <- cast TInt <$> (evalLiteral s =<< mej)
    = setCapture s e (getCaptureSlice i (Just j) s)
    | Capture (CSlice ei Nothing) <- e
    , Just (RTInt i) <- cast TInt <$> evalLiteral s ei
    = setCapture s e (getCaptureSlice i Nothing s)
    | Func l args rt fe <- e
    , isNothing l
    = let
        fns = maybe
          []
          (\k ->
            [ (k, RTFunc l (rtArgOfArg <$> args) rt fe) | not (funcExists k s) ]
          )
          l
        (appliedArgs, leftArgs) = splitAt ((rtLength . stLast) s) args
        vars                    = assignVars (stLast s) appliedArgs
        doRun                   = null leftArgs
        stack'                  = (fns <> vars) : stStack s
        s'                      = s { stLast = RTNil, stStack = stack' }
        Rd rfe s'' fe'          = runReducer rStaticFuncExp s' fe
      in
        Rd rfe s' { stStack = fns : stStack s } (Func l leftArgs rt fe')
    | Func l args rt fe <- e
    = let Rd rfe s' fe' = runReducer rStaticFuncExp (s { stLast = RTNil }) fe
      in  Rd rfe s' (Func l args rt fe')
    | Flow p q <- e
    = let Rd rp s'  p' = aux s p
          Rd rq s'' q' = aux s' q
      in  Rd (rp && rq) s (Flow p' q')
    | Program p q <- e
    = let Rd rp s'  p' = aux s { stLast = RTNil } p
          Rd rq s'' q' = aux (s' { stLast = RTNil }) q
      in  Rd (rp && rq) s'' (Program p' q')
    | Io _ <- e
    = Rd False s e
    | otherwise
    = Rd False s e
