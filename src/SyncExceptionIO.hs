{-# LANGUAGE ScopedTypeVariables #-}

module SyncExceptionIO
  ( trySync
  , runSync
  ) where

import Control.Exception
import Control.Monad.Except

{-# INLINE trySync #-}
trySync :: IO a -> ExceptT SomeException IO a
trySync m =
  ExceptT $ do
    r <- try m
    case r of
      Left e ->
        case fromException e of
          Just (_ :: SomeAsyncException) -> throwIO e
          _ -> pure $ Left e
      Right a -> pure $ Right a

{-# INLINE runSync #-}
runSync :: ExceptT SomeException IO a -> IO a
runSync m = do
  r <- runExceptT m
  case r of
    Left e -> throwIO e
    Right a -> pure a
