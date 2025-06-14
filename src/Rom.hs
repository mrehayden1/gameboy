module Rom (
  loadRom
) where

import Data.Array.Base
import Data.Array.IO.Internals
import Foreign
import Foreign.C
import GHC.Exts  (MutableByteArray#, RealWorld)
import GHC.IO.Handle
import System.IO

-- Load a ROM into memory.
--
-- Largely copied from:
-- https://hackage.haskell.org/package/array-0.5.8.0/docs/src/Data.Array.IO.html#hGetArray
loadRom :: FilePath -> IOUArray Word16 Word8 -> IO ()
loadRom filename (IOUArray (STUArray _ _ _ ptr)) = do
  withFile filename ReadMode $ \handle ->
    -- we would like to read directly into the buffer, but we can't
    -- be sure that the MutableByteArray# is pinned, so we have to
    -- allocate a separate area of memory and copy.
    allocaBytes 0xffff $ \p -> do
      r <- hGetBuf handle p 0xffff
      _ <- memcpy_ba_ptr ptr p (fromIntegral r)
      return ()

foreign import ccall unsafe "memcpy"
  memcpy_ba_ptr :: MutableByteArray# RealWorld -> Ptr a -> CSize -> IO (Ptr ())
