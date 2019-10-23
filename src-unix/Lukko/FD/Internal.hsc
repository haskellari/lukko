{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
module Lukko.FD.Internal (
    FD (..),
    fdOpen,
    fdClose,
    handleToFd,
    ) where

#include <unistd.h>
#include <fcntl.h>

import Data.Bits          ((.|.))
import Foreign.C.Error    (throwErrnoIfMinus1Retry)
import Foreign.C.Types
import Foreign.C.String   (CString, withCString)
import System.IO          (Handle)
import System.Posix.Types (CMode (..))

import qualified GHC.IO.FD        as GHC (FD (..))

#if MIN_VERSION_base(4,10,0)
import qualified GHC.IO.Handle.FD as GHC (handleToFd)
#else
import Control.Concurrent.MVar (readMVar)
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import GHC.IO.Exception
import Data.Typeable (cast)
#endif

-- | Opaque /file descriptor/
--
-- An @int@ on unix systems,
-- and @HANDLE@ on windows.
--
newtype FD = FD CInt

foreign import ccall interruptible "open"
   c_open :: CString -> CInt -> CMode -> IO CInt

foreign import ccall interruptible "close"
   c_close :: CInt -> IO CInt

-- | Open file to be used for locking.
--
-- @
-- open(path, O_RDWR | O_CREAT);
-- @
fdOpen :: FilePath -> IO FD
fdOpen fp = withCString fp $ \cfp -> do
    fd <- throwErrnoIfMinus1Retry "open" $ c_open cfp flags mode
    return (FD fd)
  where
    flags = #{const O_RDWR} .|. #{const O_CREAT}
    mode  = CMode 0o666

-- | Close lock file.
--
-- @
-- close(fd);
-- @
fdClose :: FD -> IO ()
fdClose (FD fd) =  do
    ret <- throwErrnoIfMinus1Retry "close" $ c_close fd
    return ()

handleToFd :: Handle -> IO FD
handleToFd h = do
    GHC.FD {GHC.fdFD = fd} <- ghcHandleToFd h
    return (FD fd)

ghcHandleToFd :: Handle -> IO GHC.FD
#if MIN_VERSION_base(4,10,0)
ghcHandleToFd = GHC.handleToFd
#else
ghcHandleToFd h = case h of
    FileHandle _ mv -> do
      Handle__{haDevice = dev} <- readMVar mv
      case cast dev of
          Just fd -> return fd
          Nothing -> throwErr "not a file descriptor"
    DuplexHandle{} -> throwErr "not a file handle"
  where
    throwErr msg = ioException $ IOError (Just h)
      InappropriateType "handleToFd" msg Nothing Nothing
#endif
