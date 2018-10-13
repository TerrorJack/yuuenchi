{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module FList
  ( FList
  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import GHC.Exts

data CQueue m a b where
  CPure :: (a -> m b) -> CQueue m a b
  CSnoc :: CQueue m a b -> (b -> m c) -> CQueue m a c

data FList a where
  FNil :: FList a
  FAppend :: FList a -> FList a -> FList a
  FPure :: a -> FList a
  FBind :: FList a -> CQueue FList a b -> FList b
  FFoldable :: Foldable t => t a -> FList a

instance Semigroup (FList a) where
  {-# INLINE (<>) #-}
  FNil <> l = l
  l <> FNil = l
  l0 <> l1 = FAppend l0 l1

instance Monoid (FList a) where
  {-# INLINE mempty #-}
  mempty = FNil

instance Functor FList where
  {-# INLINE fmap #-}
  fmap _ FNil = FNil
  fmap f (FPure a) = FPure (f a)
  fmap f (FBind m q) = FBind m (q `CSnoc` (FPure . f))
  fmap f l = FBind l (CPure (FPure . f))

instance Foldable FList where
  foldMap _ FNil = mempty
  foldMap f (FAppend l0 l1) = foldMap f l0 <> foldMap f l1
  foldMap f (FPure a) = f a
  foldMap f (FBind m (CPure g)) = foldMap (foldMap f . g) m
  foldMap f (FBind m (CSnoc q g)) = foldMap (foldMap f . g) (FBind m q)
  foldMap f (FFoldable l) = foldMap f l

instance Applicative FList where
  {-# INLINE pure #-}
  pure = FPure
  {-# INLINE (<*>) #-}
  FNil <*> _ = FNil
  _ <*> FNil = FNil
  FPure f <*> l = f <$> l
  f <*> l = FBind f (CPure (<$> l))

instance Alternative FList where
  {-# INLINE empty #-}
  empty = FNil
  {-# INLINE (<|>) #-}
  (<|>) = (<>)

instance Monad FList where
  {-# INLINE (>>=) #-}
  FNil >>= _ = FNil
  FPure a >>= f = f a
  FBind m q >>= f = FBind m (q `CSnoc` f)
  m >>= f = FBind m (CPure f)

instance MonadPlus FList

instance IsList (FList a) where
  type Item (FList a) = a
  {-# INLINE fromList #-}
  fromList = FFoldable
  {-# INLINE toList #-}
  toList = flip appEndo [] . foldMap (Endo . (:))

instance Show a => Show (FList a) where
  {-# INLINE showsPrec #-}
  showsPrec i = showsPrec i . toList
