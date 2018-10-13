{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module DeBruijn
  ( Expr(..)
  , DBI(..)
  , toDBI
  ) where

import Control.Applicative

class Expr t where
  lam :: (t -> t) -> t
  app :: t -> t -> t

data DBI
  = Var Int
  | Lam DBI
  | App DBI
        DBI
  deriving (Eq, Show)

instance Expr (Int -> DBI) where
  lam f i = Lam (f (\j -> Var (j - i - 1)) (i + 1))
  app = liftA2 App

toDBI ::
     (forall t. Expr t =>
                  t)
  -> DBI
toDBI = ($ (0 :: Int))
