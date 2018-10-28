module OneShotIO
  ( oneshot
  ) where

import Control.Monad
import Data.IORef
import System.IO

{-# INLINE oneshot #-}
oneshot :: IO a -> IO (IO a)
oneshot m = do
  ref <-
    fixIO $ \ref ->
      newIORef $ do
        a <- m
        atomicWriteIORef ref $ pure a
        pure a
  pure $ join $ readIORef ref
