{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module StrictByteStringBuilder
  ( Builder
  , sizeOfBuilder
  , runBuilder
  , runBuilderOnPtr
  , fromStorable
  , fromPtr
  , fromStrictByteString
  , fromLazyByteString
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Unsafe
import Data.String
import Foreign
import GHC.ForeignPtr

data Builder =
  Builder Int
          (Ptr () -> IO ())

instance Semigroup Builder where
  {-# INLINE (<>) #-}
  Builder 0 _ <> b = b
  b <> Builder 0 _ = b
  Builder l0 b0 <> Builder l1 b1 =
    Builder (l0 + l1) (\(!p) -> b0 p *> b1 (p `plusPtr` l0))

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty = Builder 0 (const (pure ()))

instance IsString Builder where
  {-# INLINE fromString #-}
  fromString = fromStrictByteString . fromString

{-# INLINE sizeOfBuilder #-}
sizeOfBuilder :: Builder -> Int
sizeOfBuilder (Builder l _) = l

{-# INLINE runBuilder #-}
runBuilder :: Builder -> IO BS.ByteString
runBuilder (Builder l b) = do
  fp <- mallocPlainForeignPtrBytes l
  withForeignPtr fp $ \p -> b p *> unsafePackCStringLen (castPtr p, l)

{-# INLINE runBuilderOnPtr #-}
runBuilderOnPtr :: Builder -> Ptr a -> IO ()
runBuilderOnPtr (Builder _ b) p = b (castPtr p)

{-# INLINE fromStorable #-}
fromStorable :: Storable a => a -> Builder
fromStorable a = Builder (sizeOf a) (flip poke a . castPtr)

{-# INLINE fromPtr #-}
fromPtr :: Ptr a -> Int -> Builder
fromPtr p l = Builder l (\p' -> copyBytes (castPtr p') p l)

{-# INLINE fromStrictByteString #-}
fromStrictByteString :: BS.ByteString -> Builder
fromStrictByteString bs =
  Builder
    (BS.length bs)
    (unsafeUseAsCStringLen bs . uncurry . copyBytes . castPtr)

{-# INLINE fromLazyByteString #-}
fromLazyByteString :: LBS.ByteString -> Builder
fromLazyByteString = LBS.foldrChunks ((<>) . fromStrictByteString) mempty
