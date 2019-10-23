-- | File locking for Windows.
module Lukko.Windows (
    FileLockingNotSupported(..),
    LockMode(..),
    hLock,
    hTryLock,
    hUnlock,
    ) where

-- TODO 
import Lukko.NoOp
