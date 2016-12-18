{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

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

-- | Mid-level FFI bindings in the 'IO' monad to [lzlib](https://www.nongnu.org/lzip/).
--
-- See also "Codec.Compression.Lzlib.ST" for the 'ST' monad version.
module Codec.Compression.Lzlib
    ( -- * Compression functions
      LzEncoder
    , CompressParams(..)
    , compressParamPreset

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

import           Data.Bits
import qualified Data.ByteString.Internal    as BS (createAndTrim)
import qualified Data.ByteString.Unsafe      as BS
import           Foreign
import           Internal
import           Prelude                     hiding (fromIntegral)

import           Codec.Compression.Lzlib.FFI

-- | Parameters for @lzip@ compressor
--
-- If 'compressDictionarySize' is 65535 and 'compressMatchLenLimit' is 16, the \"fast variant\" of LZMA is chosen.
--
data CompressParams = CompressParams
    { compressDictionarySize :: !Int
      -- ^ Valid values range from 4 KiB to 512 MiB; will be rounded
      -- upwards by adding up to @('compressDictionarySize' / 8)@ to
      -- match size supported by @lzip@ format.
    , compressMatchLenLimit  :: !Int
      -- ^ Valid values range from 5 to 273 bytes.
    , compressMemberSize     :: !Word64
      -- ^ Valid values range from 100 kB to 2 PiB.
    }

-- | Construct 'CompressParams' based on the standard preset levels used by the @lzip@ command-line interface.
--
-- The table below shows the parameters as a function of the level input argument:
--
-- +-------+--------------------------+-------------------------+
-- | level | 'compressDictionarySize' | 'compressMatchLenLimit' |
-- +=======+==========================+=========================+
-- |  ≤0   | 65535 bytes              | 16 bytes                |
-- +-------+--------------------------+-------------------------+
-- |   1   | 1 MiB                    | 5 bytes                 |
-- +-------+--------------------------+-------------------------+
-- |   2   | 1.5 MiB                  | 6 bytes                 |
-- +-------+--------------------------+-------------------------+
-- |   3   | 2 MiB                    | 8 bytes                 |
-- +-------+--------------------------+-------------------------+
-- |   4   | 3 MiB                    | 12 bytes                |
-- +-------+--------------------------+-------------------------+
-- |   5   | 4 MiB                    | 20 bytes                |
-- +-------+--------------------------+-------------------------+
-- |   6   | 8 MiB                    | 36 bytes                |
-- +-------+--------------------------+-------------------------+
-- |   7   | 16 MiB                   | 68 bytes                |
-- +-------+--------------------------+-------------------------+
-- |   8   | 24 MiB                   | 132 bytes               |
-- +-------+--------------------------+-------------------------+
-- |  ≥9   | 32 MiB                   | 273 bytes               |
-- +-------+--------------------------+-------------------------+
--
-- 'compressMemberSize' is set to its maximum allowed value (i.e. 2 PiB) for all compression levels.
--
-- __NOTE__: The \"0\" preset parameters will cause the encoder to use the \"fast variant\" of the LZMA algorithm.
--
compressParamPreset :: Int -> CompressParams
compressParamPreset lvl = case (max 0 lvl) of
    0 -> CompressParams 0xffff           16 msz
    1 -> CompressParams (1 `shiftL` 20)   5 msz
    2 -> CompressParams (3 `shiftL` 19)   6 msz
    3 -> CompressParams (1 `shiftL` 21)   8 msz
    4 -> CompressParams (3 `shiftL` 20)  12 msz
    5 -> CompressParams (1 `shiftL` 22)  20 msz
    6 -> CompressParams (1 `shiftL` 23)  36 msz
    7 -> CompressParams (1 `shiftL` 24)  68 msz
    8 -> CompressParams (3 `shiftL` 23) 132 msz
    _ -> CompressParams (1 `shiftL` 25) 273 msz
  where
    msz = 0x0008000000000000 -- 2 PiB

-- | Retrieve current error state of the encoder.
--
-- NOTE: This is not part of the exposed mid-level API as it must only be used right after an operation signalled failure
lzCompressErrno :: LzEncoder -> IO LzErrno
lzCompressErrno (LzEncoder fp) = withForeignPtr fp $ \encPtr -> toLzErrno <$> c'LZ_compress_errno encPtr

-- | Construct new 'LzEncoder'.
--
-- If a 'LzEncoder' was constructed succesfully it will be in the 'LzOk' state (as reported by 'lzCompressErrno').
--
-- __NOTE__: 'lzCompressClose' will be invoked automatically when 'LzEncoder' is garbage collected.
lzCompressOpen :: CompressParams -> IO (Either LzErrno LzEncoder)
lzCompressOpen CompressParams{..} = runExceptT $ do
    unless (c'lzlib_version_check == 0) $
      throwE LzUnknown

    p <- maybe (throwE LzMemError) pure =<< liftE allocEncoder

    eno <- liftE (lzCompressErrno p)
    unless (eno == LzOk) $ do
        let (LzEncoder fp) = p
        liftE (finalizeForeignPtr fp)
        throwE eno

    pure p
  where
    -- critical section
    allocEncoder = mask_ $ do
      p <- c'LZ_compress_open (int2cint compressDictionarySize)
                              (int2cint compressMatchLenLimit)
                              (intCast compressMemberSize)
      case () of
        _ | p == nullPtr -> pure Nothing
          | otherwise -> do
              !fp <- newForeignPtr cp'LZ_compress_close p
              pure (Just (LzEncoder fp))

-- | Promptly finalize a 'LzEncoder'.
--
-- It is not necessary to invoke 'lzCompressClose' explicitly as it
-- will be invoked implicitly when a 'LzEncoder' is garbage collected.
--
-- See also 'lzCompressOpen'.
lzCompressClose :: LzEncoder -> IO ()
lzCompressClose (LzEncoder fp) = finalizeForeignPtr fp

-- | Push uncompressed data into the encoder. The return value is the number of bytes actually consumed.
lzCompressWrite :: LzEncoder -> ByteString -> IO Int
lzCompressWrite lze@(LzEncoder fp) ibs = do
    written <- withForeignPtr fp $ \encPtr -> do
                 BS.unsafeUseAsCStringLen ibs $ \(ibsptr, ibslen) -> do
                   c'LZ_compress_write encPtr (castPtr ibsptr) (int2cint ibslen)

    when (written < 0) $ throwIO =<< lzCompressErrno lze
    pure (intCast written)

-- | Retrieve up to /n/ bytes of the compressed stream from the encoder.
--
-- Returns the empty 'ByteString' when the output buffer has been drained.
lzCompressRead :: LzEncoder -> Int -> IO ByteString
lzCompressRead lze@(LzEncoder fp) bufsize0
  = BS.createAndTrim (intCast bufsize) $ \bufptr -> do
      used <- withForeignPtr fp $ \encPtr -> c'LZ_compress_read encPtr bufptr bufsize
      when (used < 0) $ throwIO =<< lzCompressErrno lze
      pure (intCast used)
  where
    bufsize = int2cint bufsize0

-- | Finalize current member.
--
-- After this operation, the output buffer has to be drained via repeated invocations of 'lzCompressRead'.
--
-- See also 'lzCompressFinished' and 'lzCompressMemberFinished'.
lzCompressFinish :: LzEncoder -> IO LzErrno
lzCompressFinish lze@(LzEncoder fp) = do
    rc <- withForeignPtr fp c'LZ_compress_finish
    if rc == 0
     then pure LzOk
     else do
       eno <- lzCompressErrno lze
       pure $! if eno /= LzOk then eno else LzUnknown

-- | Start a new member in a multimember compression stream.
--
-- Must only be called when 'lzCompressMemberFinished' is 'True'.
lzCompressRestartMember :: LzEncoder -> Word64 -> IO LzErrno
lzCompressRestartMember lze@(LzEncoder fp) memberSize = do
    rc <- withForeignPtr fp $ \encPtr -> c'LZ_compress_restart_member encPtr (intCast memberSize)
    if rc == 0
     then pure LzOk
     else do
       eno <- lzCompressErrno lze
       pure $! if eno /= LzOk then eno else LzUnknown

-- | Force the encoder to output the compressed stream for all the uncompressed input data.
--
-- After this operation, the output buffer has to be drained via repeated invocations of 'lzCompressRead'.
--
lzCompressSyncFlush :: LzEncoder -> IO LzErrno
lzCompressSyncFlush lze@(LzEncoder fp) = do
    rc <- withForeignPtr fp c'LZ_compress_sync_flush
    if rc == 0
     then pure LzOk
     else do
       eno <- lzCompressErrno lze
       pure $! if eno /= LzOk then eno else LzUnknown


-- | Returns 'True' if the output buffer has been drained completely (which implies 'lzCompressMemberFinished').
lzCompressFinished :: LzEncoder -> IO Bool
lzCompressFinished lze@(LzEncoder fp) = do
    rc <- withForeignPtr fp c'LZ_compress_finished
    case rc of
      0 -> pure False
      1 -> pure True
      _ -> throwIO =<< lzCompressErrno lze

-- | Returns 'True' if the output buffer has been drained completely and 'lzCompressRestartMember' can be invoked.
lzCompressMemberFinished :: LzEncoder -> IO Bool
lzCompressMemberFinished lze@(LzEncoder fp) = do
    rc <- withForeignPtr fp c'LZ_compress_member_finished
    case rc of
      0 -> pure False
      1 -> pure True
      _ -> throwIO =<< lzCompressErrno lze







----------------------------------------------------------------------------

-- | Retrieve current error state of the decoder.
--
-- NOTE: This is not part of the exposed mid-level API as it must only be used right after an operation signalled failure
lzDecompressErrno :: LzDecoder -> IO LzErrno
lzDecompressErrno (LzDecoder fp) = withForeignPtr fp $ \encPtr -> toLzErrno <$> c'LZ_decompress_errno encPtr

-- | Construct new 'LzDecoder'.
--
-- If a 'LzDecoder' was constructed succesfully it will be in the 'LzOk' state (as reported by 'lzDecompressErrno').
--
-- __NOTE__: 'lzDecompressClose' will be invoked automatically when 'LzDecoder' is garbage collected.
lzDecompressOpen :: IO (Either LzErrno LzDecoder)
lzDecompressOpen = runExceptT $ do
    unless (c'lzlib_version_check == 0) $
      throwE LzUnknown

    p <- maybe (throwE LzMemError) pure =<< liftE allocDecoder

    eno <- liftE (lzDecompressErrno p)
    unless (eno == LzOk) $ do
        let (LzDecoder fp) = p
        liftE (finalizeForeignPtr fp)
        throwE eno

    pure p
  where
    -- critical section
    allocDecoder = mask_ $ do
      p <- c'LZ_decompress_open
      case () of
        _ | p == nullPtr -> pure Nothing
          | otherwise -> do
              !fp <- newForeignPtr cp'LZ_decompress_close p
              pure (Just (LzDecoder fp))

-- | Promptly finalize a 'LzDecoder'.
--
-- It is not necessary to invoke 'lzDecompressClose' explicitly as it
-- will be invoked implicitly when a 'LzDecoder' is garbage collected.
--
-- See also 'lzDecompressOpen'.
lzDecompressClose :: LzDecoder -> IO ()
lzDecompressClose (LzDecoder fp) = finalizeForeignPtr fp


-- | Push compressed data into the decoder. The return value is the number of bytes actually consumed.
lzDecompressWrite :: LzDecoder -> ByteString -> IO Int
lzDecompressWrite lze@(LzDecoder fp) ibs = do
    written <- withForeignPtr fp $ \encPtr -> do
                 BS.unsafeUseAsCStringLen ibs $ \(ibsptr, ibslen) -> do
                   c'LZ_decompress_write encPtr (castPtr ibsptr) (int2cint ibslen)

    when (written < 0) $ throwIO =<< lzDecompressErrno lze
    pure (intCast written)

-- | Retrieve up to /n/ bytes of the decompressed stream from the decoder.
--
-- Returns the empty 'ByteString' when the output buffer has been drained.
lzDecompressRead :: LzDecoder -> Int -> IO ByteString
lzDecompressRead lze@(LzDecoder fp) bufsize0
  = BS.createAndTrim (intCast bufsize) $ \bufptr -> do
      used <- withForeignPtr fp $ \encPtr -> c'LZ_decompress_read encPtr bufptr bufsize
      when (used < 0) $ throwIO =<< lzDecompressErrno lze
      pure (intCast used)
  where
    bufsize = int2cint bufsize0


-- | Instruct decoder to discard data of current member and skip till next member.
--
-- This is a no-op if the decoder is already at the start of a member.
--
lzDecompressSyncToMember :: LzDecoder -> IO LzErrno
lzDecompressSyncToMember lze@(LzDecoder fp) = do
    rc <- withForeignPtr fp c'LZ_decompress_sync_to_member
    if rc == 0
     then pure LzOk
     else do
       eno <- lzDecompressErrno lze
       pure $! if eno /= LzOk then eno else LzUnknown

-- | Returns 'True' if the output buffer has been drained completely (which implies 'lzDecompressMemberFinished').
lzDecompressFinished :: LzDecoder -> IO Bool
lzDecompressFinished lze@(LzDecoder fp) = do
    rc <- withForeignPtr fp c'LZ_decompress_finished
    case rc of
      0 -> pure False
      1 -> pure True
      _ -> throwIO =<< lzDecompressErrno lze

-- | Returns 'True' if the output buffer has been drained completely and 'lzDecompressRestartMember' can be invoked.
lzDecompressMemberFinished :: LzDecoder -> IO Bool
lzDecompressMemberFinished lze@(LzDecoder fp) = do
    rc <- withForeignPtr fp c'LZ_decompress_member_finished
    case rc of
      0 -> pure False
      1 -> pure True
      _ -> throwIO =<< lzDecompressErrno lze

-- | Finalize current member.
--
-- After this operation, the output buffer has to be drained via repeated invocations of 'lzDecompressRead'.
--
-- See also 'lzDecompressFinished' and 'lzDecompressMemberFinished'.
lzDecompressFinish :: LzDecoder -> IO LzErrno
lzDecompressFinish lze@(LzDecoder fp) = do
    rc <- withForeignPtr fp c'LZ_decompress_finish
    if rc == 0
     then pure LzOk
     else do
       eno <- lzDecompressErrno lze
       pure $! if eno /= LzOk then eno else LzUnknown

-- | Reset 'LzEncoder' into the initial state (as if 'lzCompressOpen' had just been invoked) and discard all data.
lzDecompressReset :: LzDecoder -> IO LzErrno
lzDecompressReset lze@(LzDecoder fp) = do
    rc <- withForeignPtr fp $ \encPtr -> c'LZ_decompress_reset encPtr
    if rc == 0
     then pure LzOk
     else do
       eno <- lzDecompressErrno lze
       pure $! if eno /= LzOk then eno else LzUnknown


----------------------------------------------------------------------------
