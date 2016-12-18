{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP     #-}

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

module Internal
    ( module Control.Applicative
    , module Control.Monad
    , module Control.Exception
    , module Data.Word
    , unsafeIOToST, ST
    , Typeable
    , ByteString

    , c'lzlib_version_check
    , LzEncoder(..)
    , LzDecoder(..)

    , intCast
    , int2cint

    , ExceptT(ExceptT), runExceptT, throwE, liftE
    ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST.Strict (ST)
import           Control.Monad.ST.Unsafe (unsafeIOToST)
import           Data.ByteString         (ByteString)
import           Data.Typeable           (Typeable)
import           Data.Word
import           Foreign
import           Foreign.C

#if defined(MIN_VERSION_int_cast)
import           Data.IntCast            (intCast)
#else
intCast :: (Integral a, Num b) => a -> b
intCast = fromIntegral
#endif

-- clamped conversion from 'Int' to 'CInt'
-- assumed invariant:   maxBound :: CInt <= maxBound :: Int@
int2cint :: Int -> CInt
int2cint = fromIntegral . min maxCInt . max 0
  where
    maxCInt :: Int
    maxCInt = intCast (maxBound :: CInt)

-- a non-zero value denotes failure
foreign import capi unsafe "hs_lzlib.h hs_lzlib_version_check" c'lzlib_version_check :: CInt

-- | @lzlib@ compressor handle.
newtype LzEncoder = LzEncoder (ForeignPtr LzEncoder)

-- | @lzlib@ decompressor handle.
newtype LzDecoder = LzDecoder (ForeignPtr LzDecoder)

----------------------------------------------------------------------------
-- local minimal ExceptT-like transformer
newtype ExceptT e m a = ExceptT (m (Either e a))

runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT (ExceptT m) = m

throwE :: Applicative m => e -> ExceptT e m a
throwE = ExceptT . pure . Left

liftE :: Applicative m => m a -> ExceptT e m a
liftE = ExceptT . liftA Right

instance Functor m => Functor (ExceptT e m) where
    fmap f = ExceptT . fmap (fmap f) . runExceptT

instance (Applicative m, Monad m) => Applicative (ExceptT e m) where
    pure = ExceptT . pure . Right
    (<*>) = ap
    m *> k = m >>= \_ -> k

instance (Applicative m, Monad m) => Monad (ExceptT e m) where
    m >>= k = ExceptT (either (pure . Left) (runExceptT . k) =<< runExceptT m)
    -- legacy
    return = pure
    (>>) = (*>)
