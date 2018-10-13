{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module FList
  ( FList
  , unfoldr
  , filter
  ) where

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Foldable hiding (toList)
import qualified Data.Foldable as Foldable
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import GHC.Exts
import Text.Show

data CQueue m a b where
  CPure :: (a -> m b) -> CQueue m a b
  CSnoc :: CQueue m a b -> (b -> m c) -> CQueue m a c

data FList a where
  FNil :: FList a
  FAppend :: FList a -> FList a -> FList a
  FPure :: a -> FList a
  FBind :: FList a -> CQueue FList a b -> FList b
  FFoldable :: Foldable t => t a -> FList a
  FUnfoldr :: (b -> Maybe (a, b)) -> b -> FList a

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
  fmap f (FUnfoldr g b) =
    FUnfoldr
      (\s ->
         case g s of
           Just (a, s') -> Just (f a, s')
           _ -> Nothing)
      b
  fmap f l = FBind l (CPure (FPure . f))

instance Foldable FList where
  foldMap _ FNil = mempty
  foldMap f (FAppend l0 l1) = foldMap f l0 <> foldMap f l1
  foldMap f (FPure a) = f a
  foldMap f (FBind m (CPure g)) = foldMap (foldMap f . g) m
  foldMap f (FBind m (CSnoc q g)) = foldMap (foldMap f . g) (FBind m q)
  foldMap f (FFoldable l) = foldMap f l
  foldMap f (FUnfoldr g b) = w b
    where
      w s =
        case g s of
          Just (a, s') -> f a <> w s'
          _ -> mempty

instance Applicative FList where
  {-# INLINE pure #-}
  pure = FPure
  {-# INLINE (<*>) #-}
  FNil <*> _ = FNil
  _ <*> FNil = FNil
  FPure f <*> l = f <$> l
  f <*> FPure a = ($ a) <$> f
  FBind m q <*> l = FBind m (q `CSnoc` (<$> l))
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
  toList = Foldable.toList

instance Show a => Show (FList a) where
  {-# INLINE showsPrec #-}
  showsPrec i = showsPrec i . toList

{-# INLINE unfoldr #-}
unfoldr :: (b -> Maybe (a, b)) -> b -> FList a
unfoldr = FUnfoldr

{-# INLINE filter #-}
filter :: (a -> Bool) -> FList a -> FList a
filter f =
  (>>= \a ->
         if f a
           then FPure a
           else FNil)
