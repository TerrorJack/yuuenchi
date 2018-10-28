module OneShotIO
  ( oneshot
  ) where

import Control.Concurrent
import Control.Monad
import Data.Function

{-# INLINE oneshot #-}
oneshot :: IO a -> IO (IO a)
oneshot m = do
  ref <- newEmptyMVar
  putMVar ref $ do
    a <- m
    putMVar ref $
      fix $ \go -> do
        putMVar ref go
        pure a
    pure a
  pure $ join $ takeMVar ref
