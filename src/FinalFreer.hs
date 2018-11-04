{-# LANGUAGE RankNTypes #-}

module FinalFreer
  ( Freer
  , liftFreer
  , hoistFreer
  , retractFreer
  , foldFreer
  ) where

import Control.Applicative
import Control.Monad.Reader

newtype Natural f g =
  Natural (forall x. f x -> g x)

newtype Freer f a = Freer
  { unFreer :: forall m. Monad m =>
                           ReaderT (Natural f m) m a
  }

instance MonadTrans Freer where
  {-# INLINE lift #-}
  lift = liftFreer

instance Functor (Freer f) where
  {-# INLINE fmap #-}
  fmap f (Freer m) = Freer (fmap f m)

instance Applicative (Freer f) where
  {-# INLINE pure #-}
  pure a = Freer (pure a)
  {-# INLINE (<*>) #-}
  Freer f <*> Freer a = Freer (f <*> a)
  {-# INLINE liftA2 #-}
  liftA2 f (Freer a) (Freer b) = Freer (liftA2 f a b)
  {-# INLINE (*>) #-}
  Freer a *> Freer b = Freer (a *> b)
  {-# INLINE (<*) #-}
  Freer a <* Freer b = Freer (a <* b)

instance Monad (Freer f) where
  {-# INLINE (>>=) #-}
  Freer m >>= f = Freer (m >>= unFreer . f)
  {-# INLINE (>>) #-}
  Freer m >> Freer m' = Freer (m >> m')
  {-# INLINE return #-}
  return a = Freer (return a)
  {-# INLINE fail #-}
  fail a = Freer (fail a)

{-# INLINE liftFreer #-}
liftFreer :: f a -> Freer f a
liftFreer m = Freer (ReaderT (\(Natural f) -> f m))

{-# INLINE hoistFreer #-}
hoistFreer :: (forall x. f x -> g x) -> Freer f a -> Freer g a
hoistFreer t (Freer m) = Freer (withReaderT (\(Natural g) -> Natural (g . t)) m)

{-# INLINE retractFreer #-}
retractFreer :: Monad m => Freer m a -> m a
retractFreer (Freer m) = runReaderT m (Natural id)

{-# INLINE foldFreer #-}
foldFreer :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a
foldFreer t (Freer m) = runReaderT m (Natural t)
