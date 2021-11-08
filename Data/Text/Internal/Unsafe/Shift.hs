{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Unsafe.Shift
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fast, unchecked bit shifting functions.

module Data.Text.Internal.Unsafe.Shift
    (
      UnsafeShift(..)
    ) where

-- import qualified Data.Bits as Bits
import GHC.Base
#if __GLASGOW_HASKELL__ >= 903
  hiding (uncheckedShiftL64#, uncheckedShiftRL64#)
#endif
import GHC.Word

-- | This is a workaround for poor optimisation in GHC 6.8.2.  It
-- fails to notice constant-width shifts, and adds a test and branch
-- to every shift.  This imposes about a 10% performance hit.
--
-- These functions are undefined when the amount being shifted by is
-- greater than the size in bits of a machine Int#.
class UnsafeShift a where
    shiftL :: a -> Int -> a
    shiftR :: a -> Int -> a

instance UnsafeShift Word16 where
    {-# INLINE shiftL #-}
    shiftL (W16# x#) (I# i#) = W16# (narrow16WordCompat# (word16ToWordCompat# x# `uncheckedShiftL#` i#))

    {-# INLINE shiftR #-}
    shiftR (W16# x#) (I# i#) = W16# (wordToWord16Compat# (word16ToWordCompat# x# `uncheckedShiftRL#` i#))

instance UnsafeShift Word32 where
    {-# INLINE shiftL #-}
    shiftL (W32# x#) (I# i#) = W32# (narrow32WordCompat# (word32ToWordCompat# x# `uncheckedShiftL#` i#))

    {-# INLINE shiftR #-}
    shiftR (W32# x#) (I# i#) = W32# (wordToWord32Compat# (word32ToWordCompat# x# `uncheckedShiftRL#` i#))

instance UnsafeShift Word64 where
    {-# INLINE shiftL #-}
    shiftL (W64# x#) (I# i#) = W64# (x# `uncheckedShiftL64#` i#)

    {-# INLINE shiftR #-}
    shiftR (W64# x#) (I# i#) = W64# (x# `uncheckedShiftRL64#` i#)

instance UnsafeShift Int where
    {-# INLINE shiftL #-}
    shiftL (I# x#) (I# i#) = I# (x# `iShiftL#` i#)

    {-# INLINE shiftR #-}
    shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)

{-
instance UnsafeShift Integer where
    {-# INLINE shiftL #-}
    shiftL = Bits.shiftL

    {-# INLINE shiftR #-}
    shiftR = Bits.shiftR
-}

#if MIN_VERSION_base(4,16,0)
word16ToWordCompat# :: Word16# -> Word#
word16ToWordCompat# = word16ToWord#

word32ToWordCompat# :: Word32# -> Word#
word32ToWordCompat# = word32ToWord#

wordToWord16Compat# :: Word# -> Word16#
wordToWord16Compat# = wordToWord16#

wordToWord32Compat# :: Word# -> Word32#
wordToWord32Compat# = wordToWord32#

narrow16WordCompat# :: Word# -> Word16#
narrow16WordCompat# = wordToWord16#

narrow32WordCompat# :: Word# -> Word32#
narrow32WordCompat# = wordToWord32#
#else
-- No-ops
word16ToWordCompat# :: Word# -> Word#
word16ToWordCompat# x = x

word32ToWordCompat# :: Word# -> Word#
word32ToWordCompat# x = x

wordToWord16Compat# :: Word# -> Word#
wordToWord16Compat# x = x

wordToWord32Compat# :: Word# -> Word#
wordToWord32Compat# x = x

-- Actual narrowing
narrow16WordCompat# :: Word# -> Word#
narrow16WordCompat# = narrow16Word#

narrow32WordCompat# :: Word# -> Word#
narrow32WordCompat# = narrow32Word#
#endif
