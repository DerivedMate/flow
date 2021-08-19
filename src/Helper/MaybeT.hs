module Helper.MaybeT where

import           Control.Applicative            ( Applicative(liftA2) )
import           Control.Monad
import           Data.Function

newtype MaybeT m a
  = MaybeT { unwrapMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f a = MaybeT $ fmap (fmap f) (unwrapMaybeT a)

instance Applicative m => Applicative (MaybeT m) where
  pure a = MaybeT (pure (Just a))
  fa <*> va = MaybeT $ liftA2 (<*>) (unwrapMaybeT fa) (unwrapMaybeT va)

instance Monad m => Monad (MaybeT m) where
  vm >>= f = MaybeT $ unwrapMaybeT vm >>= (unwrapMaybeT . aux)
   where
    aux (Just v) = f v
    aux vmb      = MaybeT (pure Nothing)
