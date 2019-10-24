{-# LANGUAGE CPP              #-}
{-# LANGUAGE InterruptibleFFI #-}
module Lukko.Internal.FillBytes (fillBytes) where

#if MIN_VERSION_base(4,8,0)
import Foreign.Marshal.Utils (fillBytes)
#else
import Data.Word       (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr     (Ptr (..))
#endif

#if !MIN_VERSION_base(4,8,0)
fillBytes :: Ptr a -> Word8 -> Int -> IO ()
fillBytes dest char size = do
    _ <- memset dest (fromIntegral char) (fromIntegral size)
    return ()

foreign import ccall unsafe "string.h" memset  :: Ptr a -> CInt  -> CSize -> IO (Ptr a)
#endif
