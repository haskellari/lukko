{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE Trustworthy #-}
-- | Linux open file descriptor locking.
-- 
-- <https://www.gnu.org/software/libc/manual/html_node/Open-File-Description-Locks.html>
--
-- We prefer this over BSD locking (e.g. flock) since the latter appears to
-- break in some NFS configurations. Note that we intentionally do not try to
-- use ordinary POSIX file locking due to its peculiar semantics under
-- multi-threaded environments.
--
module Lukko.OFD (
    -- * Types
    FileLockingNotSupported(..),
    fileLockingSupported,
    FileLockingMethod (..),
    fileLockingMethod,
    LockMode(..),
    -- * File descriptors
    FD,
    fdOpen,
    fdClose,
    fdLock,
    fdTryLock,
    fdUnlock,
    -- * Handles
    hLock,
    hTryLock,
    hUnlock,
    ) where

#define _GNU_SOURCE
#include <unistd.h>
#include <fcntl.h>

import Control.Monad (void)
import System.IO (Handle)

import Data.Function
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.IO.Exception
import GHC.Ptr
import System.Posix.Types (COff, CPid)

import Lukko.Internal.FD
import Lukko.Internal.FillBytes
import Lukko.Internal.Types

-------------------------------------------------------------------------------
-- Support constants
-------------------------------------------------------------------------------

-- | A constants specifying whether file locking is supported.
fileLockingSupported :: Bool
fileLockingSupported = True

-- | A constant specifying this method
fileLockingMethod :: FileLockingMethod
fileLockingMethod = MethodOFD

-------------------------------------------------------------------------------
-- FD
-------------------------------------------------------------------------------

-- | Lock using OFD locks.
fdLock :: FD -> LockMode -> IO ()
fdLock fd mode = void (lockImpl Nothing fd "fdLock" mode True)

-- | Try to lock using OFD locks.
fdTryLock :: FD -> LockMode -> IO Bool
fdTryLock fd mode = lockImpl Nothing fd "fdTryLock" mode False

-- | Unlock using OFD locks.
fdUnlock :: FD -> IO ()
fdUnlock = unlockImpl

-------------------------------------------------------------------------------
-- Handle
-------------------------------------------------------------------------------

-- | Lock using OFD locks.
hLock :: Handle -> LockMode -> IO ()
hLock h mode = do
    fd <- handleToFd h
    void (lockImpl (Just h) fd "hLock" mode True)

-- | Try to lock using OFD locks.
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock h mode = do
    fd <- handleToFd h
    lockImpl (Just h) fd "hTryLock" mode False

-- | Unlock using OFD locks.
hUnlock :: Handle -> IO ()
hUnlock h = do
    fd <- handleToFd h
    unlockImpl fd

-------------------------------------------------------------------------------
-- Compat
-------------------------------------------------------------------------------

-- there is no alignment in old hsc2hs
#let alignmentcompat t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

foreign import ccall interruptible "fcntl"
  c_fcntl :: CInt -> CInt -> Ptr FLock -> IO CInt

data FLock  = FLock { l_type   :: CShort
                    , l_whence :: CShort
                    , l_start  :: COff
                    , l_len    :: COff
                    , l_pid    :: CPid
                    }

instance Storable FLock where
    sizeOf _ = #{size struct flock}
    alignment _ = #{alignmentcompat struct flock}
    poke ptr x = do
        fillBytes ptr 0 (sizeOf x)
        #{poke struct flock, l_type}   ptr (l_type x)
        #{poke struct flock, l_whence} ptr (l_whence x)
        #{poke struct flock, l_start}  ptr (l_start x)
        #{poke struct flock, l_len}    ptr (l_len x)
        #{poke struct flock, l_pid}    ptr (l_pid x)
    peek ptr = do
        x1 <- #{peek struct flock, l_type}   ptr
        x2 <- #{peek struct flock, l_whence} ptr
        x3 <- #{peek struct flock, l_start}  ptr
        x4 <- #{peek struct flock, l_len}    ptr
        x5 <- #{peek struct flock, l_pid}    ptr
        return (FLock x1 x2 x3 x4 x5)

lockImpl :: Maybe Handle -> FD -> String -> LockMode -> Bool -> IO Bool
lockImpl mh (FD fd) ctx mode block = do
  with flock $ \flock_ptr -> fix $ \retry -> do
      ret <- c_fcntl fd mode' flock_ptr
      case ret of
        0 -> return True
        _ -> getErrno >>= \errno -> case () of
          _ | not block && errno == eWOULDBLOCK -> return False
            | errno == eINTR -> retry
            | otherwise -> ioException $ errnoToIOError ctx errno mh Nothing
  where
    flock = FLock { l_type = case mode of
                               SharedLock -> #{const F_RDLCK}
                               ExclusiveLock -> #{const F_WRLCK}
                  , l_whence = #{const SEEK_SET}
                  , l_start = 0
                  , l_len = 0
                  , l_pid = 0
                  }
    mode'
      | block     = #{const F_OFD_SETLKW}
      | otherwise = #{const F_OFD_SETLK}

unlockImpl :: FD -> IO ()
unlockImpl (FD fd) = do
  let flock = FLock { l_type = #{const F_UNLCK}
                    , l_whence = #{const SEEK_SET}
                    , l_start = 0
                    , l_len = 0
                    , l_pid = 0
                    }
  throwErrnoIfMinus1_ "hUnlock"
      $ with flock $ c_fcntl fd #{const F_OFD_SETLK}
