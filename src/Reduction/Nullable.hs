module Reduction.Nullable where
import           Data.Function
import           Data.Maybe                     ( isNothing )
import           Debug.Pretty.Simple            ( pTraceShow
                                                , pTraceShowId
                                                )
import           Eval.RT
import           Reduction.Reducer
import           Reduction.Static
import           Syntax
import           Text.Pretty.Simple

{-
  Given an expression in context (State),
  determine if it's nullable (=not executed/-able or ignored).
  If so, return Nil (+ optional info for the parent);
  otherwise, return the (potentially modified) node.


  forall A in M. A nullable -> M nullable 
-}


{- Observations:
  1. Compile time falsy conditionals:
    { ~f: a =
      False | - a 1
      > a 3 | a
    }

    is equivalent to:
    { ~f: a =
      > a 3 | a
    }

    since `False` is never met.
    Execution:
      1.  Determine if condition is always falsy (nullable + False)
      2a. If it is, (with `Cond c e rest`) return `nullable rest`
      2b. Otherwise, return `Cond c' ( nullable e ) ( nullable rest )` 

  2. Compile time truthy conditionals:
    { ~f: a = 
      True | a
           | 1
    }

    is equivalent to:
    { ~f: a = a } 

    All cases after the nullable one are
    removed.
    Execution:
      1.  Determine if condition is always truthy (nullable + True)
      2a. If it is, return `Cond c' ( nullable e ) FNil`
      2b. Otherwise, return `Cond c' ( nullable e ) ( reduce next_fe )`  
-}

rTestFile :: [Reducer Exp Bool] -> FilePath -> IO ()
rTestFile reds path = parseFile path >>= pPrint . fmap aux . prExp
 where
  aux :: Exp -> Exp
  aux x = foldl (\e r -> rdExp $ runReducer r emptyState e) x reds


rNullableFuncExp :: Reducer FuncExp Bool
rNullableFuncExp = Reducer aux
 where
  staticallyBool :: Exp -> Maybe Bool
  staticallyBool e | LBool b <- e = Just b
                   | otherwise    = Nothing

  aux :: State -> FuncExp -> Rd FuncExp Bool
  aux s e
    | Cond c ee next <- e, Just True <- staticallyBool c = Rd
      False
      s
      (Single $ rdExp $ runReducer rNullableExp s ee)
    | Cond c ee next <- e, Just False <- staticallyBool c = aux s next
    | Cond c ee next <- e = Rd
      False
      s
      (Cond c (rdExp $ runReducer rNullableExp s ee) (rdExp $ aux s next))
    | Single ee <- e = Rd False
                          s
                          (Single . rdExp $ runReducer rNullableExp s ee)
    | otherwise = Rd False s e

rNullableExp :: Reducer Exp Bool
rNullableExp = Reducer aux
 where
  aux :: State -> Exp -> Rd Exp Bool
  aux s e
    | Nil <- e
    = Rd True s e
    | Cell m ee <- e
    = let Rd red s' ee' = aux s ee in Rd red s' (Cell m ee')
    | Func l args fExp <- e
    = let Rd red s' fExp' = runReducer rNullableFuncExp s fExp
      in  case (fExp', null args && isNothing l) of
            (Single ee, True) -> Rd red s' ee
            _                 -> Rd red s' (Func l args fExp')
    | Flow p q <- e
    = let Rd redP sp' p' = aux s p
          Rd redQ sq' q' = aux sp' q
      in  case (redP, redQ, ignoresInput q') of
            (_    , _   , True) -> Rd True s q'
            (True , True, _   ) -> Rd True s Nil
            (False, True, _   ) -> Rd False sp' p'
            _                   -> Rd False sq' (Flow p' q')
    | Program p q <- e
    = let Rd redP sp' p' = aux (s { stLast = RTNil }) p
          Rd redQ sq' q' = aux (sp' { stLast = RTNil }) q
      in  Rd False sq' { stLast = RTNil } (Program p' q')
    |
      -- pTraceShow (("----< P >----", p, p'), ("----< Q >----", q, q'))
      otherwise
    = Rd False s e

  ignoresInput :: Exp -> Bool
  ignoresInput e | Io (IoStdIn _) <- e = True
                 | LInt _ <- e         = True
                 | LBool _ <- e        = True
                 | LString _ <- e      = True
                 | LFloat _ <- e       = True
                 | LList ls <- e       = all ignoresInput ls
                 | LTuple ls <- e      = all ignoresInput ls
                 | Cell _ ee <- e      = ignoresInput ee
                 | Flow ee _ <- e      = ignoresInput ee
                 | Func _ args _ <- e  = null args
                 | otherwise           = False

