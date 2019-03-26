{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compact
  ( writeWithCompact
  , readWithCompact
  ) where

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Foldable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Compact
import GHC.Compact.Serialized
import GHC.ForeignPtr
import Prelude hiding (fail)
import System.IO

hGetBuf' :: Handle -> Ptr a -> Int -> IO ()
hGetBuf' h p l = do
  r <- hGetBuf h p l
  if r == l
    then pure ()
    else fail $
         "Asterius.Internals.Compact.hGetBuf': expected to read " <> show l <>
         " bytes, got " <>
         show r

hPutStorable :: Storable a => Handle -> a -> IO ()
hPutStorable h v = do
  fp <- mallocPlainForeignPtr
  withForeignPtr fp $ \p -> do
    poke p v
    hPutBuf h p (sizeOf v)

hGetStorable ::
     forall a. Storable a
  => Handle
  -> IO a
hGetStorable h = do
  fp <- mallocPlainForeignPtr
  withForeignPtr fp $ \p -> do
    hGetBuf' h p (sizeOf (undefined :: a))
    peek p

hPutCompact :: Handle -> Compact a -> IO ()
hPutCompact h c =
  withSerializedCompact c $ \SerializedCompact {..} -> do
    hPutStorable h (length serializedCompactBlockList)
    for_ serializedCompactBlockList $ \(p, l) -> do
      hPutStorable h p
      hPutStorable h l
    hPutStorable h serializedCompactRoot
    for_ serializedCompactBlockList $ \(p, l) -> hPutBuf h p (fromIntegral l)

hGetCompact :: Handle -> IO (Compact a)
hGetCompact h = do
  sc <-
    do l <- hGetStorable h
       bl <- replicateM l $ (,) <$> hGetStorable h <*> hGetStorable h
       r <- hGetStorable h
       pure
         SerializedCompact
           {serializedCompactBlockList = bl, serializedCompactRoot = r}
  Just c <- importCompact sc $ \p l -> hGetBuf' h p (fromIntegral l)
  pure c

writeCompact :: FilePath -> Compact a -> IO ()
writeCompact p c = withBinaryFile p WriteMode $ \h -> hPutCompact h c

readCompact :: FilePath -> IO (Compact a)
readCompact p = withBinaryFile p ReadMode hGetCompact

writeWithCompact :: FilePath -> a -> IO ()
writeWithCompact p = (>>= writeCompact p) . compact

readWithCompact :: FilePath -> IO a
readWithCompact = (getCompact <$>) . readCompact
