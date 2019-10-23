{-# LANGUAGE CPP #-}
module Lukko.Internal.HandleToFD (ghcHandleToFd) where

import System.IO (Handle)
import qualified GHC.IO.FD as GHC (FD (..))

#if MIN_VERSION_base(4,10,0)
import qualified GHC.IO.Handle.FD as GHC (handleToFd)
#else
import Control.Concurrent.MVar (readMVar)
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import GHC.IO.Exception
import Data.Typeable (cast)
#endif

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
