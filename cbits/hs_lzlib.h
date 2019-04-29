// Copyright (C) 2016  Herbert Valerio Riedel
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HS_LZLIB_H_
#define HS_LZLIB_H_

#include <stddef.h>
#include <stdint.h>
#include <lzlib.h>

#if LZ_API_VERSION != 1
# error unsupported LZ_API_VERSION
#endif

static inline int
hs_lzlib_version_check(void)
{
  const char *ver = LZ_version();

  if (!ver)
    return 1;

  if (ver[0] != LZ_version_string[0])
    return 1;

  return 0;
}

static void
hs_lzlib_compress_close(struct LZ_Encoder *const encoder)
{
  LZ_compress_close(encoder);
}

static void
hs_lzlib_decompress_close(struct LZ_Decoder *const decoder)
{
  LZ_decompress_close(decoder);
}

static inline void*
hs_lzlib_decompress_close_addr(void)
{
  return &hs_lzlib_decompress_close;
}

static inline void*
hs_lzlib_compress_close_addr(void)
{
  return &hs_lzlib_compress_close;
}

#endif
