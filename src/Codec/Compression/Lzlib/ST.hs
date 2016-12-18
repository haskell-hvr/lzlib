{-# LANGUAGE Trustworthy #-}

-- Copyright (C) 2019  Herbert Valerio Riedel
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | @lzlib@ operations in the 'ST' monad
--
-- See respective functions in "Codec.Compression.Lzlib" for documentation where missing.
module Codec.Compression.Lzlib.ST
    ( -- * Compression functions
      LzEncoder
    , IO.CompressParams(..)
    , IO.compressParamPreset

    , lzCompressOpen
    , lzCompressClose
    , lzCompressRead
    , lzCompressWrite
    , lzCompressSyncFlush
    , lzCompressFinish
    , lzCompressFinished
    , lzCompressMemberFinished
    , lzCompressRestartMember

      -- * Decompression functions
    , LzDecoder

    , lzDecompressOpen
    , lzDecompressClose
    , lzDecompressRead
    , lzDecompressWrite
    , lzDecompressSyncToMember
    , lzDecompressFinish
    , lzDecompressFinished
    , lzDecompressMemberFinished
    , lzDecompressReset

      -- * Error codes
    , LzErrno(..)
    ) where

import           Codec.Compression.Lzlib (LzErrno (..))
import qualified Codec.Compression.Lzlib as IO
import           Internal                hiding (LzDecoder, LzEncoder)

----------------------------------------------------------------------------

newtype LzEncoder s = LzEncoder IO.LzEncoder

lzCompressOpen :: IO.CompressParams -> ST s (Either LzErrno (LzEncoder s))
lzCompressOpen p = unsafeIOToST (fmap LzEncoder <$> IO.lzCompressOpen p)

lzCompressClose :: LzEncoder s -> ST s ()
lzCompressClose (LzEncoder lze) = unsafeIOToST (IO.lzCompressClose lze)

lzCompressWrite :: LzEncoder s -> ByteString -> ST s Int
lzCompressWrite (LzEncoder lze) ibs = unsafeIOToST (IO.lzCompressWrite lze ibs)

lzCompressRead :: LzEncoder s -> Int -> ST s ByteString
lzCompressRead (LzEncoder lze) bufsize = unsafeIOToST (IO.lzCompressRead lze bufsize)

lzCompressFinish :: LzEncoder s -> ST s LzErrno
lzCompressFinish (LzEncoder lze) = unsafeIOToST (IO.lzCompressFinish lze)

lzCompressRestartMember :: LzEncoder s -> Word64 -> ST s LzErrno
lzCompressRestartMember (LzEncoder lze) msize = unsafeIOToST (IO.lzCompressRestartMember lze msize)

lzCompressSyncFlush :: LzEncoder s -> ST s LzErrno
lzCompressSyncFlush (LzEncoder lze) = unsafeIOToST (IO.lzCompressSyncFlush lze)

lzCompressFinished :: LzEncoder s -> ST s Bool
lzCompressFinished (LzEncoder lze) = unsafeIOToST (IO.lzCompressFinished lze)

lzCompressMemberFinished :: LzEncoder s -> ST s Bool
lzCompressMemberFinished (LzEncoder lze) = unsafeIOToST (IO.lzCompressMemberFinished lze)

----------------------------------------------------------------------------

newtype LzDecoder s = LzDecoder IO.LzDecoder

lzDecompressOpen :: ST s (Either LzErrno (LzDecoder s))
lzDecompressOpen = unsafeIOToST (fmap LzDecoder <$> IO.lzDecompressOpen)

lzDecompressClose :: LzDecoder s -> ST s ()
lzDecompressClose (LzDecoder lze) = unsafeIOToST (IO.lzDecompressClose lze)

lzDecompressWrite :: LzDecoder s -> ByteString -> ST s Int
lzDecompressWrite (LzDecoder lze) ibs = unsafeIOToST (IO.lzDecompressWrite lze ibs)

lzDecompressRead :: LzDecoder s -> Int -> ST s ByteString
lzDecompressRead (LzDecoder lze) bufsize = unsafeIOToST (IO.lzDecompressRead lze bufsize)

lzDecompressFinish :: LzDecoder s -> ST s LzErrno
lzDecompressFinish (LzDecoder lze) = unsafeIOToST (IO.lzDecompressFinish lze)

lzDecompressReset :: LzDecoder s -> ST s LzErrno
lzDecompressReset (LzDecoder lze) = unsafeIOToST (IO.lzDecompressReset lze)

lzDecompressSyncToMember :: LzDecoder s -> ST s LzErrno
lzDecompressSyncToMember (LzDecoder lze) = unsafeIOToST (IO.lzDecompressSyncToMember lze)

lzDecompressFinished :: LzDecoder s -> ST s Bool
lzDecompressFinished (LzDecoder lze) = unsafeIOToST (IO.lzDecompressFinished lze)

lzDecompressMemberFinished :: LzDecoder s -> ST s Bool
lzDecompressMemberFinished (LzDecoder lze) = unsafeIOToST (IO.lzDecompressMemberFinished lze)
