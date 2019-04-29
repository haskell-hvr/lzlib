{-# LANGUAGE CApiFFI            #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Unsafe             #-}

{-# OPTIONS_GHC -fno-warn-dodgy-foreign-imports #-}

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

-- | Primitive unsafe low-level FFI bindings to @lzlib@
module Codec.Compression.Lzlib.FFI
    ( -- * Parameter limits
      --
      -- | See also <https://www.nongnu.org/lzip/manual/lzlib_manual.html#Parameter-limits chapter "4 Parameter limits"> in the @lzlib@ manual.

      c'LZ_min_dictionary_bits
    , c'LZ_min_dictionary_size
    , c'LZ_max_dictionary_size
    , c'LZ_max_dictionary_bits
    , c'LZ_min_match_len_limit
    , c'LZ_max_match_len_limit

      -- * Compression functions
      --
      -- | See also <https://www.nongnu.org/lzip/manual/lzlib_manual.html#Compression-functions chapter "5 Compression functions"> in the @lzlib@ manual.

    , LzEncoder

    , c'LZ_compress_open
    , c'LZ_compress_close
    , cp'LZ_compress_close

    , c'LZ_compress_finish
    , c'LZ_compress_restart_member
    , c'LZ_compress_sync_flush

    , c'LZ_compress_read
    , c'LZ_compress_write
    , c'LZ_compress_write_size

    , c'LZ_compress_errno
    , c'LZ_compress_finished
    , c'LZ_compress_member_finished

    , c'LZ_compress_data_position
    , c'LZ_compress_member_position
    , c'LZ_compress_total_in_size
    , c'LZ_compress_total_out_size

      -- * Decompression functions
      --
      -- | See also <https://www.nongnu.org/lzip/manual/lzlib_manual.html#Decompression-functions chapter "6 Decompression functions"> in the @lzlib@ manual.

    , LzDecoder

    , c'LZ_decompress_open
    , c'LZ_decompress_close
    , cp'LZ_decompress_close

    , c'LZ_decompress_finish
    , c'LZ_decompress_reset
    , c'LZ_decompress_sync_to_member

    , c'LZ_decompress_read
    , c'LZ_decompress_write
    , c'LZ_decompress_write_size

    , c'LZ_decompress_errno
    , c'LZ_decompress_finished
    , c'LZ_decompress_member_finished

    , c'LZ_decompress_member_version
    , c'LZ_decompress_dictionary_size
    , c'LZ_decompress_data_crc

    , c'LZ_decompress_data_position
    , c'LZ_decompress_member_position
    , c'LZ_decompress_total_in_size
    , c'LZ_decompress_total_out_size

      -- * Error codes
      --
      -- | See also <https://www.nongnu.org/lzip/manual/lzlib_manual.html#Error-codes chapter "7 Error codes"> in the @lzlib@ manual.

    , C'LZ_Errno
    , LzErrno(..), toLzErrno

      -- * Internal
    , c'lzlib_version_check
    ) where

import           Foreign
import           Foreign.C
import           Internal

#include "hs_lzlib.h"

-- a non-zero value denotes failure
foreign import capi unsafe "hs_lzlib.h hs_lzlib_version_check" c'lzlib_version_check :: CInt

----------------------------------------------------------------------------
-- Parameter limits

foreign import capi unsafe "hs_lzlib.h LZ_min_dictionary_bits" c'LZ_min_dictionary_bits :: CInt
foreign import capi unsafe "hs_lzlib.h LZ_min_dictionary_size" c'LZ_min_dictionary_size :: CInt
foreign import capi unsafe "hs_lzlib.h LZ_max_dictionary_bits" c'LZ_max_dictionary_bits :: CInt
foreign import capi unsafe "hs_lzlib.h LZ_max_dictionary_size" c'LZ_max_dictionary_size :: CInt
foreign import capi unsafe "hs_lzlib.h LZ_min_match_len_limit" c'LZ_min_match_len_limit :: CInt
foreign import capi unsafe "hs_lzlib.h LZ_max_match_len_limit" c'LZ_max_match_len_limit :: CInt

----------------------------------------------------------------------------
-- Compression functions

foreign import capi "hs_lzlib.h LZ_compress_open"    c'LZ_compress_open  :: CInt -> CInt -> CULLong -> IO (Ptr LzEncoder)
foreign import capi "hs_lzlib.h LZ_compress_close"   c'LZ_compress_close :: Ptr LzEncoder -> IO CInt
-- NB: we ignore the retval to match the type-sig of 'newForeignPtr'
foreign import capi "hs_lzlib.h hs_lzlib_compress_close_addr" cp'LZ_compress_close :: FunPtr (Ptr LzEncoder -> IO ())


foreign import capi "hs_lzlib.h LZ_compress_finish"         c'LZ_compress_finish         :: Ptr LzEncoder -> IO CInt
foreign import capi "hs_lzlib.h LZ_compress_restart_member" c'LZ_compress_restart_member :: Ptr LzEncoder -> CULLong -> IO CInt
foreign import capi "hs_lzlib.h LZ_compress_sync_flush"     c'LZ_compress_sync_flush     :: Ptr LzEncoder -> IO CInt


foreign import capi "hs_lzlib.h LZ_compress_read"           c'LZ_compress_read           :: Ptr LzEncoder -> Ptr Word8 -> CInt -> IO CInt
foreign import capi "hs_lzlib.h LZ_compress_write"          c'LZ_compress_write          :: Ptr LzEncoder -> Ptr Word8 -> CInt -> IO CInt
foreign import capi unsafe "hs_lzlib.h LZ_compress_write_size" c'LZ_compress_write_size  :: Ptr LzEncoder -> IO CInt


foreign import capi unsafe "hs_lzlib.h LZ_compress_errno"            c'LZ_compress_errno           :: Ptr LzEncoder -> IO C'LZ_Errno
foreign import capi unsafe "hs_lzlib.h LZ_compress_finished"         c'LZ_compress_finished        :: Ptr LzEncoder -> IO CInt -- 0=False, 1=True
foreign import capi unsafe "hs_lzlib.h LZ_compress_member_finished"  c'LZ_compress_member_finished :: Ptr LzEncoder -> IO CInt


foreign import capi unsafe "hs_lzlib.h LZ_compress_data_position"    c'LZ_compress_data_position   :: Ptr LzEncoder -> IO CULLong
foreign import capi unsafe "hs_lzlib.h LZ_compress_member_position"  c'LZ_compress_member_position :: Ptr LzEncoder -> IO CULLong
foreign import capi unsafe "hs_lzlib.h LZ_compress_total_in_size"    c'LZ_compress_total_in_size   :: Ptr LzEncoder -> IO CULLong
foreign import capi unsafe "hs_lzlib.h LZ_compress_total_out_size"   c'LZ_compress_total_out_size  :: Ptr LzEncoder -> IO CULLong

----------------------------------------------------------------------------
-- Decompression functions

foreign import capi "hs_lzlib.h LZ_decompress_open"    c'LZ_decompress_open  :: IO (Ptr LzDecoder)
foreign import capi "hs_lzlib.h LZ_decompress_close"   c'LZ_decompress_close :: Ptr LzDecoder -> IO CInt
-- NB: we ignore the retval to match the type-sig of 'newForeignPtr'
foreign import capi "hs_lzlib.h hs_lzlib_decompress_close_addr" cp'LZ_decompress_close :: FunPtr (Ptr LzDecoder -> IO ())

foreign import capi "hs_lzlib.h LZ_decompress_finish"         c'LZ_decompress_finish         :: Ptr LzDecoder -> IO CInt
foreign import capi "hs_lzlib.h LZ_decompress_reset"          c'LZ_decompress_reset          :: Ptr LzDecoder -> IO CInt
foreign import capi "hs_lzlib.h LZ_decompress_sync_to_member" c'LZ_decompress_sync_to_member :: Ptr LzDecoder -> IO CInt


foreign import capi "hs_lzlib.h LZ_decompress_read"           c'LZ_decompress_read           :: Ptr LzDecoder -> Ptr Word8 -> CInt -> IO CInt
foreign import capi "hs_lzlib.h LZ_decompress_write"          c'LZ_decompress_write          :: Ptr LzDecoder -> Ptr Word8 -> CInt -> IO CInt
foreign import capi unsafe "hs_lzlib.h LZ_decompress_write_size" c'LZ_decompress_write_size  :: Ptr LzDecoder -> IO CInt


foreign import capi unsafe "hs_lzlib.h LZ_decompress_errno"            c'LZ_decompress_errno           :: Ptr LzDecoder -> IO C'LZ_Errno
foreign import capi unsafe "hs_lzlib.h LZ_decompress_finished"         c'LZ_decompress_finished        :: Ptr LzDecoder -> IO CInt -- 0=False, 1=True
foreign import capi unsafe "hs_lzlib.h LZ_decompress_member_finished"  c'LZ_decompress_member_finished :: Ptr LzDecoder -> IO CInt


foreign import capi unsafe "hs_lzlib.h LZ_decompress_member_version"  c'LZ_decompress_member_version  :: Ptr LzDecoder -> IO CInt
foreign import capi unsafe "hs_lzlib.h LZ_decompress_dictionary_size" c'LZ_decompress_dictionary_size :: Ptr LzDecoder -> IO CInt
foreign import capi unsafe "hs_lzlib.h LZ_decompress_data_crc"        c'LZ_decompress_data_crc        :: Ptr LzDecoder -> IO CUInt


foreign import capi unsafe "hs_lzlib.h LZ_decompress_data_position"   c'LZ_decompress_data_position   :: Ptr LzDecoder -> IO CULLong
foreign import capi unsafe "hs_lzlib.h LZ_decompress_member_position" c'LZ_decompress_member_position :: Ptr LzDecoder -> IO CULLong
foreign import capi unsafe "hs_lzlib.h LZ_decompress_total_in_size"   c'LZ_decompress_total_in_size   :: Ptr LzDecoder -> IO CULLong
foreign import capi unsafe "hs_lzlib.h LZ_decompress_total_out_size"  c'LZ_decompress_total_out_size  :: Ptr LzDecoder -> IO CULLong

----------------------------------------------------------------------------
-- Error codes

type C'LZ_Errno = #{type enum LZ_Errno}

-- | @lzlib@ error codes
--
-- See <https://www.nongnu.org/lzip/manual/lzlib_manual.html#Error-codes lzlib manual> for more details.
data LzErrno
    = LzOk
    | LzBadArgument
    | LzMemError
    | LzSequenceError
    | LzHeaderError
    | LzUnexpectedEof
    | LzDataError
    | LzLibraryError
    | LzUnknown -- ^ not defined by @lzlib@
    deriving (Eq,Show,Typeable)

instance Exception LzErrno

-- | Convert FFI @enum LZ_Errno@ into the Haskell enum type 'LzErrno'.
toLzErrno :: C'LZ_Errno -> LzErrno
toLzErrno eno = case eno of
   #{const LZ_ok}             -> LzOk
   #{const LZ_bad_argument}   -> LzBadArgument
   #{const LZ_mem_error}      -> LzMemError
   #{const LZ_sequence_error} -> LzSequenceError
   #{const LZ_header_error}   -> LzHeaderError
   #{const LZ_unexpected_eof} -> LzUnexpectedEof
   #{const LZ_data_error}     -> LzDataError
   #{const LZ_library_error}  -> LzLibraryError
   _                          -> LzUnknown
