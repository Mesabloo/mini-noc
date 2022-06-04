{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ < 930
{-# LANGUAGE DataKinds #-}
#endif

#include "MachDeps.h"

module Variables where

import GHC.Exts (Int#, MutableByteArray#, State#, newByteArray#, readIntArray#, writeIntArray#, (+#), (-#))
import GHC.Types (Type, UnliftedType)

-- | A strict, unlifted, boxed mutable variable to an 'Int#' value.
type MutableIntVar# :: Type -> UnliftedType
newtype MutableIntVar# s = MutableIntVar# (MutableByteArray# s)

-- | Creates a new mutable variable containing the default 'Int#' given.
newIntVar# :: Int# -> State# s -> (# State# s, MutableIntVar# s #)
newIntVar# value s0 =
  let !(# s1, arr #) = newByteArray# WORD_SIZE_IN_BITS# s0
      !s2 = writeIntArray# arr 0# value s1
   in (# s2, MutableIntVar# arr #)
{-# INLINEABLE newIntVar# #-}

-- | Returns the value currently held in the variable.
readIntVar# :: MutableIntVar# s -> State# s -> (# State# s, Int# #)
readIntVar# (MutableIntVar# arr) = readIntArray# arr 0#
{-# INLINE readIntVar# #-}

-- | Writes a new value into the variable.
writeIntVar# :: MutableIntVar# s -> Int# -> State# s -> State# s
writeIntVar# (MutableIntVar# arr) = writeIntArray# arr 0#
{-# INLINE writeIntVar# #-}

-- | Increments a variable by one and returns its new value.
incrementAndGetIntVar# :: MutableIntVar# s -> State# s -> (# State# s, Int# #)
incrementAndGetIntVar# var s0 =
  let !(# s1, val #) = readIntVar# var s0
      !valPlusOne = val +# 1#
      !s2 = writeIntVar# var valPlusOne s1
   in (# s2, valPlusOne #)
{-# INLINEABLE incrementAndGetIntVar# #-}

-- | Decrements the variable by one, and returns its old value.
getAndDecrementIntVar# :: MutableIntVar# s -> State# s -> (# State# s, Int# #)
getAndDecrementIntVar# var s0 =
  let !(# s1, val #) = readIntVar# var s0
      !valMinusOne = val -# 1#
      !s2 = writeIntVar# var valMinusOne s1
   in (# s2, val #)
{-# INLINEABLE getAndDecrementIntVar# #-}
