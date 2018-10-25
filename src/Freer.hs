{-# LANGUAGE RankNTypes #-}

module Freer
  ( Freer
  , liftFreer
  , hoistFreer
  , retractFreer
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

newtype Freer f a = Freer
  { unFreer :: forall r. (a -> r) -> (forall b. (b -> r) -> f b -> r) -> r
  }

instance MonadTrans Freer where
  {-# INLINE lift #-}
  lift = liftFreer

instance Functor (Freer f) where
  {-# INLINE fmap #-}
  fmap = liftA

instance Applicative (Freer f) where
  {-# INLINE pure #-}
  pure a = Freer (\p _ -> p a)
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Freer f) where
  {-# INLINE (>>=) #-}
  Freer m >>= f = Freer (\p g -> m (\a -> unFreer (f a) p g) g)

{-# INLINE liftFreer #-}
liftFreer :: f a -> Freer f a
liftFreer m = Freer (\p f -> f p m)

{-# INLINE hoistFreer #-}
hoistFreer :: (forall x. f x -> g x) -> Freer f a -> Freer g a
hoistFreer t (Freer m) = Freer (\p f -> m p (\g h -> f g (t h)))

{-# INLINE retractFreer #-}
retractFreer :: Monad m => Freer m a -> m a
retractFreer (Freer m) = m pure (=<<)
