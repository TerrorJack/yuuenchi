module OneShotIO
  ( oneshot
  ) where

import System.IO.Unsafe

{-# INLINE oneshot #-}
oneshot :: IO a -> IO (IO a)
oneshot m = do
  a <- unsafeInterleaveIO m
  pure $ pure a
