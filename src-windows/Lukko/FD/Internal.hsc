{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
module Lukko.FD.Internal (
    FD (..),
    fdOpen,
    fdClose,
    handleToFd,
    ) where

import System.IO (Handle)
import GHC.Windows (iNVALID_HANDLE_VALUE, HANDLE, LPWSTR, BOOL, getLastError, failWith)
import Foreign.C.Types (CInt (..))
import Foreign.C.Error (throwErrnoIf)
import Foreign.C.String (withCWString)
import Foreign.Ptr (Ptr)

import qualified GHC.IO.FD        as GHC (FD (..))

import Lukko.Internal.HandleToFD (ghcHandleToFd)

newtype FD = FD HANDLE

fdOpen :: FilePath -> IO FD
fdOpen fp = withCWString fp $ \cfp -> do
    fw <- c_fdOpen cfp
    if fw /= iNVALID_HANDLE_VALUE
    then return (FD fw)
    else getLastError >>= failWith "fdOpen"

fdClose :: FD -> IO ()
fdClose (FD fw) = do
    r <- c_CloseHandle fw
    if r
    then return ()
    else getLastError >>= failWith "fdClose"

handleToFd :: Handle -> IO FD
handleToFd h = do
    GHC.FD {GHC.fdFD = fd} <- ghcHandleToFd h
    wh <- throwErrnoIf (== iNVALID_HANDLE_VALUE) "handleToFd" $ c_get_osfhandle fd
    return (FD wh)

-- https://msdn.microsoft.com/en-us/library/aa297958.aspx
foreign import ccall unsafe "_get_osfhandle"
  c_get_osfhandle :: CInt -> IO HANDLE

-- Opening file is complicated
foreign import ccall interruptible "fdOpen"
  c_fdOpen :: LPWSTR -> IO HANDLE

#if defined(i386_HOST_ARCH)

-- https://docs.microsoft.com/en-gb/windows/win32/api/handleapi/nf-handleapi-closehandle
foreign import stdcall interruptible "CloseHandle"
  c_CloseHandle :: HANDLE -> IO BOOL

#elif defined(x86_64_HOST_ARCH)

-- https://docs.microsoft.com/en-gb/windows/win32/api/handleapi/nf-handleapi-closehandle
foreign import ccall interruptible "CloseHandle"
  c_CloseHandle :: HANDLE -> IO BOOL

#else
#error Unknown mingw32 arch
#endif
