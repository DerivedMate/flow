module Reduction.Reducer where

import           Data.Maybe
import           Debug.Pretty.Simple            ( pTraceShowId )
import           Eval.RT
import           Syntax

data RState = RState

data Rd e a = Rd
  { rdVal   :: a
  , rdState :: State
  , rdExp   :: e
  }
  deriving (Show, Eq)

instance Functor (Rd e) where
  fmap g r = r { rdVal = (g . rdVal) r }


newtype Reducer e a = Reducer { runReducer :: State -> e -> Rd e a}

instance Functor (Reducer e) where
  fmap g r =
    Reducer $ \s e -> let rd = runReducer r s e in rd { rdVal = g $ rdVal rd }

instance Applicative (Reducer e) where
  pure a = Reducer $ \s e -> Rd { rdVal = a, rdExp = e, rdState = s }
  rf <*> rv = Reducer $ \s e ->
    let Rd f s'  e'  = runReducer rf s e
        Rd v s'' e'' = runReducer rv s' e'
    in  Rd (f v) s'' e''

instance Monad (Reducer e) where
  rv >>= f = Reducer
    $ \s e -> let Rd v s' e' = runReducer rv s e in runReducer (f v) s' e'


rVoid :: Reducer e ()
rVoid = Reducer $ \s e -> Rd { rdVal = (), rdState = s, rdExp = e }

optimize :: Reducer Exp v -> Exp -> Exp
optimize r e = rdExp $ runReducer r emptyState e
