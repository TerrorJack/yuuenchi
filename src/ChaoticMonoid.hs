module ChaoticMonoid
  ( Chaotic
  , toChaotic
  , runChaotic
  , simplyChaotic
  , dontBeChaotic
  ) where

import System.IO.Unsafe
import System.Random

newtype Chaotic a =
  Chaotic (IO Bool -> IO a)

instance Semigroup a => Semigroup (Chaotic a) where
  {-# INLINE (<>) #-}
  Chaotic c0 <> Chaotic c1 =
    Chaotic
      (\g -> do
         b <- g
         if b
           then (<>) <$> c1 g <*> c0 g
           else (<>) <$> c0 g <*> c1 g)

instance Monoid a => Monoid (Chaotic a) where
  {-# INLINE mempty #-}
  mempty = toChaotic mempty

{-# INLINE toChaotic #-}
toChaotic :: a -> Chaotic a
toChaotic = Chaotic . const . pure

{-# INLINE runChaotic #-}
runChaotic :: IO Bool -> Chaotic a -> a
runChaotic g (Chaotic c) = unsafePerformIO (c g)

{-# INLINE simplyChaotic #-}
simplyChaotic :: Chaotic a -> a
simplyChaotic = runChaotic randomIO

{-# INLINE dontBeChaotic #-}
dontBeChaotic :: Chaotic a -> a
dontBeChaotic = runChaotic (pure False)
