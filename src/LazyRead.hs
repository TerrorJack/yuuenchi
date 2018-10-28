{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LazyRead
  ( ReadChunks(..)
  , lazyRead
  ) where

import Control.Exception
import Data.Functor
import OneShotIO
import System.IO.Unsafe

data ReadChunks s a = ReadChunks
  { emptyChunks :: s
  , prependChunk :: a -> s -> s
  , isEmptyChunk :: a -> Bool
  , readChunk :: IO a
  , finalize :: IO ()
  }

{-# INLINE lazyRead #-}
lazyRead :: ReadChunks s a -> IO s
lazyRead ReadChunks {..} = do
  finalize_once <- oneshot finalize
  let lazy_go = unsafeInterleaveIO go
      go =
        flip onException finalize_once $ do
          c <- readChunk
          if isEmptyChunk c
            then finalize_once $> emptyChunks
            else prependChunk c <$> lazy_go
  lazy_go
