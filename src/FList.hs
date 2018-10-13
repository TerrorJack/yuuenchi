{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module FList
  ( FList
  , unfoldr
  , filter
  , reverse
  ) where

import Control.Applicative
import Control.Monad
import Data.Binary
import qualified Data.Foldable as Foldable
import Data.Monoid
import GHC.Exts
import Language.Haskell.TH.Syntax
import Prelude hiding (filter, reverse)

data FList a where
  FNil :: FList a
  FAppend :: FList a -> FList a -> FList a
  FPure :: a -> FList a
  FBind :: FList a -> (a -> FList b) -> FList b
  FFoldable :: Foldable t => t a -> FList a
  FUnfoldr :: (b -> Maybe (a, b)) -> b -> FList a
  FReverse :: FList a -> FList a

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
  fmap f (FBind m c) = FBind m (fmap f . c)
  fmap f (FUnfoldr g b) =
    FUnfoldr
      (\s ->
         case g s of
           Just (a, s') -> Just (f a, s')
           _ -> Nothing)
      b
  fmap f l = FBind l (FPure . f)

instance Foldable FList where
  foldMap _ FNil = mempty
  foldMap f (FAppend l0 l1) = foldMap f l0 <> foldMap f l1
  foldMap f (FPure a) = f a
  foldMap f (FBind m c) = foldMap (foldMap f . c) m
  foldMap f (FFoldable l) = foldMap f l
  foldMap f (FUnfoldr g b) = w b
    where
      w s =
        case g s of
          Just (a, s') -> f a <> w s'
          _ -> mempty
  foldMap f (FReverse l) = getDual (foldMap (Dual . f) l)

instance Applicative FList where
  {-# INLINE pure #-}
  pure = FPure
  {-# INLINE (<*>) #-}
  FNil <*> _ = FNil
  _ <*> FNil = FNil
  FPure f <*> l = f <$> l
  f <*> FPure a = ($ a) <$> f
  FBind m c <*> l = FBind m ((<$> l) <=< c)
  f <*> l = FBind f (<$> l)

instance Alternative FList where
  {-# INLINE empty #-}
  empty = FNil
  {-# INLINE (<|>) #-}
  (<|>) = (<>)

instance Monad FList where
  {-# INLINE (>>=) #-}
  FNil >>= _ = FNil
  FPure a >>= f = f a
  FBind m q >>= f = FBind m (f <=< q)
  m >>= f = FBind m f

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

instance Eq a => Eq (FList a) where
  l0 == l1 = toList l0 == toList l1

instance Ord a => Ord (FList a) where
  compare l0 l1 = compare (toList l0) (toList l1)

instance Binary a => Binary (FList a) where
  {-# INLINE get #-}
  get = fromList <$> get
  {-# INLINE put #-}
  put = put . toList

instance Lift a => Lift (FList a) where
  {-# INLINE lift #-}
  lift l = do
    xs <- traverse lift (toList l)
    pure
      (AppE
         (VarE
            (Name
               (OccName "fromList")
               (NameG VarName (PkgName "base") (ModName "GHC.Exts"))))
         (ListE xs))

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

{-# INLINE reverse #-}
reverse :: FList a -> FList a
reverse = FReverse
