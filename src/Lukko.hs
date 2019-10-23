{-# LANGUAGE CPP #-}
-- | Open 'Handle' based locking
module Lukko (
    FileLockingNotSupported(..),
    Impl.fileLockingSupported,
    LockMode(..),
    -- * File descriptors
    Impl.FD,
    Impl.fdOpen,
    Impl.fdClose,
    fdLock,
    fdTryLock,
    fdUnlock,
    -- * Handles
    hLock,
    hTryLock,
    hUnlock,
    ) where

import Control.Monad (void)
import System.IO (Handle)

import Lukko.Internal

#if defined(USE_OFD_LOCKING)
import qualified Lukko.OFD as Impl
#endif

-------------------------------------------------------------------------------
-- Handles
-------------------------------------------------------------------------------

-- | If a 'Handle' references a file descriptor, attempt to lock contents of the
-- underlying file in appropriate mode. If the file is already locked in
-- incompatible mode, this function blocks until the lock is established. The
-- lock is automatically released upon closing a 'Handle'.
--
-- Things to be aware of:
--
-- 1) This function may block inside a C call. If it does, in order to be able
-- to interrupt it with asynchronous exceptions and/or for other threads to
-- continue working, you MUST use threaded version of the runtime system.
--
-- 2) The implementation uses 'LockFileEx' on Windows and 'flock' otherwise,
-- hence all of their caveats also apply here.
--
-- 3) On non-Windows plaftorms that don't support 'flock' (e.g. Solaris) this
-- function throws 'FileLockingNotImplemented'. We deliberately choose to not
-- provide fcntl based locking instead because of its broken semantics.
--
hLock :: Handle -> LockMode -> IO ()
hLock = Impl.hLock

-- | Non-blocking version of 'hLock'.
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock = Impl.hTryLock

-- | Release a lock taken with 'hLock' or 'hTryLock'.
hUnlock :: Handle -> IO ()
hUnlock = Impl.hUnlock

-------------------------------------------------------------------------------
-- File descriptors
-------------------------------------------------------------------------------

-- | Like 'hLock', but work on "raw" file descriptor,
-- as handled by 'fdOpen' and 'fdClose'.
fdLock :: Impl.FD -> LockMode -> IO ()
fdLock = Impl.fdLock

-- | Non-blocking version of 'fdLock'.
fdTryLock :: Impl.FD -> LockMode -> IO Bool
fdTryLock = Impl.fdTryLock

-- | Release a lock taken with 'fdLock' or 'fdTryLock'.
fdUnlock :: Impl.FD -> IO ()
fdUnlock = Impl.fdUnlock
