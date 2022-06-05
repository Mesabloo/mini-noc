{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Runtime.Internal
  ( -- * Stacks

    -- ** Call stack
    CallStack# (..),
    newCallStack#,
    pushCallStack#,
    popCallStack#,
    resizeCallStack#,
#if DEBUG == 1
    debugCallStack#,
#endif

    -- ** Data stack
    DataStack# (..),
    newDataStack#,
    pushDataStack#,
    popDataStack#,
    peekDataStack#,
    freezeDataStack#,
    resizeDataStack#,
#if DEBUG == 1
    debugDataStack#,
#endif

    -- * Values
    Bool# (.., False#, True#),
    showBool#,
    Value# (.., VQuote#, VDouble#, VInteger#, VCharacter#, VBoolean#),
    showValue#,
    eqValue#,
    decodeValue0#,
    decodeValue1#,
    encodeValue0#,
    Closure,

    -- * Exceptions
    StackUnderflow (..),
  )
where

#include "MachDeps.h"

#define WORD_SIZE_IN_BYTES (WORD_SIZE_IN_BITS# `quotInt#` 8# )

#define VALUE_SIZE_IN_BYTES (1# +# 4# )

#define INITIAL_CALLSTACK_SIZE 20
#define INITIAL_DATASTACK_SIZE 100
#define STACK_SAFE_OPERATIONS 0

#if STACK_SAFE_OPERATIONS == 1
import Control.Exception (toException)
#endif
#if DEBUG == 1
import Data.Function (($))
#endif
import Data.Semigroup ((<>))
import Data.String (String)
import GHC.Exception (Exception)
import GHC.Exts (Char (C#), Char#, Int (I#), Int#, MutableByteArray#, RealWorld, State#, eqChar#, getSizeofMutableByteArray#, newByteArray#, quotInt#, readIntArray#, resizeMutableByteArray#, writeIntArray#, (*#), (+#), (==#), (>=#), ByteArray#, Float#, Float (F#), eqFloat#, readWord8Array#, word8ToWord#, unsafeCoerce#, copyMutableByteArray#, unsafeFreezeByteArray#, Word32#, readWord8ArrayAsWord32#, writeWord8Array#, writeWord8ArrayAsWord32#, wordToWord8#, indexWord8Array#, indexWord8ArrayAsWord32#, Word8#, word2Int#, word32ToWord#)
#if STACK_SAFE_OPERATIONS == 1
import GHC.Exts (raise#)
#endif
#if DEBUG == 1
import GHC.Exts (indexIntArray#, unsafeFreezeByteArray#, (-#), newPinnedByteArray#, copyMutableByteArray#)
import GHC.IO (unsafePerformIO, unIO)
#endif
import GHC.Show (Show, show)
#if DEBUG == 1
import System.IO (putStrLn, putStr)
#endif
import GHC.Types (RuntimeRep (..), TYPE, UnliftedRep, Type)
import Variables (MutableIntVar#, getAndDecrementIntVar#, incrementAndGetIntVar#, newIntVar#, readIntVar#)
import GHC.Err (undefined)

-- | Tried popping from an empty stack.
type StackUnderflow :: Type
data StackUnderflow = StackUnderflow
  deriving stock (Show)

instance Exception StackUnderflow

------------------------------------------------------------------------
----------------------------- STACKS -----------------------------------
------------------------------------------------------------------------

-- | Holds 'Int#'s internally, which are simply offsets from the beginning of the @CODE@ section.
type CallStack# :: Type -> TYPE ('TupleRep '[UnliftedRep, UnliftedRep])
newtype CallStack# s = CallStack (# MutableByteArray# s, MutableIntVar# s #)

-- | Allocates a new empty call stack with initially INITIAL_CALLSTACK_SIZE slots.
newCallStack# :: State# s -> (# State# s, CallStack# s #)
newCallStack# s0 =
  let !(# s1, ptr #) = newIntVar# -1# s0
      !(# s2, arr #) = newByteArray# (INITIAL_CALLSTACK_SIZE# *# WORD_SIZE_IN_BYTES) s1
   in (# s2, CallStack (# arr, ptr #) #)
{-# INLINE newCallStack# #-}

-- | Inserts a new code offset on top of the call stack.
--
--   If the stack is not big enough to hold the new value, it will be resized.
--   This is why a new 'CallStack#' object is returned, as resizing may not be done in-place.
--
--   /Warning:/ You must not use the array which was passed to this function after it returns.
pushCallStack# :: CallStack# s -> Int# -> State# s -> (# State# s, CallStack# s #)
pushCallStack# (CallStack (# arr, ptr #)) offset s0 =
  let !(# s1, arr0 #) = resizeCallStack# arr ptr s0
      !(# s2, ptrPlusOne #) = incrementAndGetIntVar# ptr s1
      !s3 = writeIntArray# arr0 ptrPlusOne offset s2
   in (# s3, CallStack (# arr0, ptr #) #)
{-# INLINEABLE pushCallStack# #-}

{- ORMOLU_DISABLE -}

-- | Pop the top of the call stack, throwing a 'StackUnderflowException' when the stack is already empty.
popCallStack# :: CallStack# RealWorld -> State# RealWorld -> (# State# RealWorld, Int# #)
popCallStack# (CallStack (# arr, ptr #)) s0 =
  let !(# s1, ptrValue #) = getAndDecrementIntVar# ptr s0
   in
#if STACK_SAFE_OPERATIONS == 1
       case ptrValue of
         -1# ->
           let _ = incrementAndGetIntVar# ptr s1
            in raise# (toException StackUnderflow)
         _ ->
#endif
           readIntArray# arr ptrValue s1
{-# INLINEABLE popCallStack# #-}

-- {- ORMOLU_ENABLE -}

-- | Resize the 'MutableByteArray#' inside a 'CallStack#' by multiplying its size by 2 if the current stack pointer
--   would go out of bounds.
--   That way, you can continue pushing values onto the stack.
--
--   Note that a new 'MutableByteArray#' is returned, as resizing may not be done in-place. (see 'resizeMutableByteArray#')
resizeCallStack# :: MutableByteArray# s -> MutableIntVar# s -> State# s -> (# State# s, MutableByteArray# s #)
resizeCallStack# arr ptr s0 =
  let !(# s1, ptrValue #) = readIntVar# ptr s0
      !(# s2, oldSize #) = getSizeofMutableByteArray# arr s1
#if DEBUG
      !_ = unsafePerformIO (putStrLn $ "Callstack: ptr=" <> show (I# ptrValue) <> ", off=" <> show (I# (ptrValue *# WORD_SIZE_IN_BYTES)) <> "B, size=" <> show (I# oldSize) <> "B")
#endif
   in case (ptrValue +# 1#) *# WORD_SIZE_IN_BYTES >=# oldSize of
        1# ->
#if DEBUG
          let !_ = unsafePerformIO (putStrLn $ "Callstack: expanding") in
#endif
          let !(# s3, !arr0 #) = resizeMutableByteArray# arr (oldSize *# 2#) s2
           in (# s3, arr0 #)
        _ ->
#if DEBUG
          let !_ = unsafePerformIO (putStrLn $ "Callstack: not expanding") in
#endif
          (# s2, arr #)
{-# INLINEABLE resizeCallStack# #-}

#if DEBUG == 1
debugCallStack# :: CallStack# RealWorld -> State# RealWorld -> State# RealWorld
debugCallStack# (CallStack (# arr, ptr #)) s0 =
  let !(# s1, _ #) = unIO (putStr "stack=[ ") s0
      !(# s2, !size #) = getSizeofMutableByteArray# arr s1
      !(# s3, !arr0 #) = newPinnedByteArray# size s2
      !s4 = copyMutableByteArray# arr 0# arr0 0# size s3
      !(# s5, !arr1 #) = unsafeFreezeByteArray# arr0 s4
      !(# s6, ptr0 #) = readIntVar# ptr s5
   in go arr1 0# (ptr0 +# 1#) s6
  where
    go arr0 off end s0 = case off >=# end of
      1# ->
        let !(# s1, _ #) = unIO (putStrLn ("], ptr=" <> show (I# (end -# 1#)))) s0
         in s1
      _ ->
        let !idx = indexIntArray# arr0 off
            !(# s1, _ #) = unIO (putStr (show (I# idx) <> " ")) s0
         in go arr0 (off +# 1#) end s1
#endif

{- ORMOLU_ENABLE -}

-----------------------------------------------------------------

-- | Holds 'Value#'s internally (these are a 'Tag#' + some value-specific encoding).
type DataStack# :: Type -> TYPE ('TupleRep '[UnliftedRep, UnliftedRep])
newtype DataStack# s = DataStack (# MutableByteArray# s, MutableIntVar# s #)

-- | Allocate a new empty data stack (holding 'Value#'s) with bytes capacity INITIAL_DATASTACK_SIZE.
newDataStack# :: State# s -> (# State# s, DataStack# s #)
newDataStack# s0 =
  let !(# s1, ptr #) = newIntVar# -1# s0
      !(# s2, arr #) = newByteArray# (INITIAL_CALLSTACK_SIZE# *# (1# +# 8#)) s1
   in (# s2, DataStack (# arr, ptr #) #)
{-# INLINE newDataStack# #-}

-- | Push a new value onto the top of the data stack.
--
--   The stack will be extended if the pointer would go out of bounds.
--   Note that the resizing may not be performed in-place, explaining why the new stack is returned.
--
--   /Warning:/ You must not use the array which was passed to this function after it returns.
pushDataStack# :: DataStack# s -> Value# -> State# s -> (# State# s, DataStack# s #)
pushDataStack# (DataStack (# arr, ptr #)) val s0 =
  let !(# s1, arr0 #) = resizeDataStack# arr ptr s0
      !(# s2, ptrPlusOne #) = incrementAndGetIntVar# ptr s1
      !s3 = encodeValue0# val arr0 ptrPlusOne s2
   in (# s3, DataStack (# arr0, ptr #) #)
{-# INLINEABLE pushDataStack# #-}

{- ORMOLU_DISABLE -}

popDataStack# :: DataStack# s -> State# s -> (# State# s, Value# #)
popDataStack# (DataStack (# arr, ptr #)) s0 =
  let !(# s1, ptrValue #) = getAndDecrementIntVar# ptr s0
   in
#if STACK_SAFE_OPERATIONS == 1
       case ptrValue of
         -1# ->
          let _ = incrementAndGetIntVar# ptr s1
           in raise# (toException StackUnderflow)
         _ ->
#endif
           decodeValue0# arr ptrValue s1
{-# INLINEABLE popDataStack# #-}

-- | Get the top of the data stack without actually removing it.
--   This may be useful to inspect data before doing anything.
peekDataStack# :: DataStack# s -> State# s -> (# State# s, Value# #)
peekDataStack# (DataStack (# arr, ptr #)) s0 =
  let !(# s1, ptrValue #) = readIntVar# ptr s0
   in
#if STACK_SAFE_OPERATIONS == 1
      case ptrValue of
        -1# ->
           let _ = incrementAndGetIntVar# ptr s1
            in raise# (toException StackUnderflow)
        _ ->
#endif
          decodeValue0# arr ptrValue s1
{-# INLINEABLE peekDataStack# #-}

{- ORMOLU_ENABLE -}

-- | Copy the whole data stack into a new immutable 'Array#' of 'Value#'s.
--
--   It is still safe to use the data stack after, as this function makes a full copy.
--
--   /Note:/ The returned array will not be as big as the capacity of the data stack, rather it will contain
--   only as much 'Value#'s which have been added onto it.
freezeDataStack# :: DataStack# s -> State# s -> (# State# s, ByteArray# #)
freezeDataStack# (DataStack (# arr, ptr #)) s0 =
  let !(# s1, ptrValue #) = readIntVar# ptr s0
      !size = (ptrValue +# 1#) *# VALUE_SIZE_IN_BYTES
      !(# s2, arr0 #) = newByteArray# size s1
      !s3 = copyMutableByteArray# arr 0# arr0 0# size s2
      !(# s4, arr1 #) = unsafeFreezeByteArray# arr0 s3
   in (# s4, arr1 #)
{-# INLINEABLE freezeDataStack# #-}

-- | TODO: documentation
resizeDataStack# :: MutableByteArray# s -> MutableIntVar# s -> State# s -> (# State# s, MutableByteArray# s #)
resizeDataStack# arr ptr s0 =
  let !(# s1, ptrValue #) = readIntVar# ptr s0
      !(# s2, oldSize #) = getSizeofMutableByteArray# arr s1
#if DEBUG
      !_ = unsafePerformIO (putStrLn $ "Datastack: ptr=" <> show (I# ptrValue) <> ", off=" <> show (I# (ptrValue *# VALUE_SIZE_IN_BYTES)) <> "B, size=" <> show (I# oldSize) <> "B")
#endif
   in case (ptrValue +# 1#) *# VALUE_SIZE_IN_BYTES >=# oldSize of
        1# ->
#if DEBUG
          let !_ = unsafePerformIO (putStrLn $ "Datastack: expanding") in
#endif
          let !(# s3, !arr0 #) = resizeMutableByteArray# arr (oldSize *# 2#) s2
           in (# s3, arr0 #)
        _ ->
#if DEBUG
          let !_ = unsafePerformIO (putStrLn $ "Datastack: not expanding") in
#endif
          (# s2, arr #)
{-# INLINEABLE resizeDataStack# #-}

#if DEBUG == 1
debugDataStack# :: DataStack# RealWorld -> State# RealWorld -> State# RealWorld
debugDataStack# (DataStack (# arr, ptr #)) s0 = s0
  -- let !(# s1, _ #) = unIO (putStr "stack=[ ") s0
  --     !(# s2, !size #) = getSizeofMutableByteArray# arr s1
  --     !(# s3, !arr0 #) = newPinnedByteArray# size s2
  --     !s4 = copyMutableByteArray# arr 0# arr0 0# size s3
  --     !(# s5, !arr1 #) = unsafeFreezeByteArray# arr0 s4
  --     !(# s6, ptr0 #) = readIntVar# ptr s5
  --  in go arr1 0# (ptr0 +# 1#) s6
  -- where
  --   go arr0 off end s0 = case off >=# end of
  --     1# ->
  --       let !(# s1, _ #) = unIO (putStrLn ("], ptr=" <> show (I# (end -# 1#)))) s0
  --        in s1
  --     _ ->
  --       let !idx = indexIntArray# arr0 off
  --           !(# s1, _ #) = unIO (putStr (show (I# idx) <> " ")) s0
  --        in go arr0 (off +# 1#) end s1
#endif


--------------------------------------------------------------------
----------------------------- VALUES -------------------------------
--------------------------------------------------------------------

-- | An unlifted, unboxed equivalent to the standard data type 'Bool', where @0#@ is false and @1#@ is true.
type Bool# :: TYPE 'IntRep
newtype Bool# = Bool# Int#

-- | Pattern synonyms for convenience.
pattern True#, False# :: Bool#
pattern True# = Bool# 1#
pattern False# = Bool# 0#

{-# COMPLETE True#, False# #-}

-- | Retrieve the 'String' representation of a 'Bool#'.
--
--   Note that this is not inside a 'Show' instance as 'Show' takes a lifted type as first parameter.
showBool# :: Bool# -> String
showBool# True# = "true"
showBool# False# = "false"
{-# INLINE showBool# #-}

-- | The kind of runtime values as an unboxed sum.
type Value# :: TYPE ('SumRep '[ 'IntRep, 'FloatRep, 'IntRep, 'WordRep, 'IntRep ])
newtype Value# = Value (# Int#| Float#| Int#| Char#| Bool# #)

pattern VQuote# :: Int# -> Value#
pattern VQuote# idx = Value (# idx | | | | #)

pattern VDouble# :: Float# -> Value#
pattern VDouble# d = Value (# | d | | | #)

pattern VInteger# :: Int# -> Value#
pattern VInteger# i = Value (# | | i | | #)

pattern VCharacter# :: Char# -> Value#
pattern VCharacter# c = Value (# | | | c | #)

pattern VBoolean# :: Bool# -> Value#
pattern VBoolean# b = Value (# | | | | b #)

{-# COMPLETE VQuote#, VDouble#, VInteger#, VCharacter#, VBoolean# #-}

-- | Returns a 'String' representation of a 'Value#'.
showValue# :: Value# -> String
showValue# (VQuote# off) = "#" <> show (I# off)
showValue# (VDouble# d) = show (F# d)
showValue# (VInteger# i) = show (I# i)
showValue# (VCharacter# c) = show (C# c)
showValue# (VBoolean# b) = showBool# b
showValue# _ = "???"
{-# INLINE showValue# #-}

-- | Tests the equality of 'Value#'s and returns '0#' if they differ, or '1#' if they are equal.
eqValue# :: Value# -> Value# -> Int#
eqValue# (VDouble# d1) (VDouble# d2) = d1 `eqFloat#` d2
eqValue# (VInteger# i1) (VInteger# i2) = i1 ==# i2
eqValue# (VCharacter# c1) (VCharacter# c2) = eqChar# c1 c2
eqValue# (VBoolean# (Bool# b1)) (VBoolean# (Bool# b2)) = b1 ==# b2
eqValue# (VQuote# o1) (VQuote# o2) = o1 ==# o2
eqValue# _ _ = 0#
{-# INLINE eqValue# #-}

decodeValue0# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Value# #)
decodeValue0# arr ptr s0 =
  let baseOffset = ptr *# VALUE_SIZE_IN_BYTES

      !(# s1, word8Tag #) = readWord8Array# arr baseOffset s0
      !(# s2, valueAsWord32 #) = readWord8ArrayAsWord32# arr (baseOffset +# 1#) s1
  in case word8ToWord# word8Tag of
    0## -> (# s2, VQuote# (unsafeCoerce# valueAsWord32) #)
    1## -> (# s2, VDouble# (unsafeCoerce# valueAsWord32) #)
    2## -> (# s2, VInteger# (unsafeCoerce# valueAsWord32) #)
    3## -> (# s2, VCharacter# (unsafeCoerce# valueAsWord32) #)
    4## -> (# s2, VBoolean# (unsafeCoerce# valueAsWord32) #)
{-# INLINEABLE decodeValue0# #-}

decodeValue1# :: ByteArray# -> Int# -> State# s -> (# State# s, Value# #)
decodeValue1# arr ptr s0 =
  let baseOffset = ptr *# VALUE_SIZE_IN_BYTES

      word8Tag = indexWord8Array# arr baseOffset
      valueAsWord32 = indexWord8ArrayAsWord32# arr (baseOffset +# 1#)
  in case word8ToWord# word8Tag of
    0## -> (# s0, VQuote# (word2Int# (word32ToWord# valueAsWord32)) #)
    1## -> (# s0, VDouble# (unsafeCoerce# valueAsWord32) #)
    2## -> (# s0, VInteger# (word2Int# (word32ToWord# valueAsWord32)) #)
    3## -> (# s0, VCharacter# (unsafeCoerce# valueAsWord32) #)
    4## -> (# s0, VBoolean# (Bool# (word2Int# (word32ToWord# valueAsWord32))) #)
{-# INLINE decodeValue1# #-}

encodeValue0# :: Value# -> MutableByteArray# s -> Int# -> State# s -> State# s
encodeValue0# val arr ptr s0 =
  let !(# tag, encoded #) = encode#

      baseOffset = ptr *# VALUE_SIZE_IN_BYTES

      s1 = writeWord8Array# arr baseOffset tag s0
   in writeWord8ArrayAsWord32# arr (baseOffset +# 1#) encoded s1
  where
    encode# :: (# Word8#, Word32# #)
    encode# = case val of
      VQuote# off -> (# wordToWord8# 0##, unsafeCoerce# off #)
      VDouble# f -> (# wordToWord8# 1##, unsafeCoerce# f #)
      VInteger# i -> (# wordToWord8# 2##, unsafeCoerce# i #)
      VCharacter# c -> (# wordToWord8# 3##, unsafeCoerce# c #)
      VBoolean# b -> (# wordToWord8# 4##, unsafeCoerce# b #)
    {-# INLINE encode# #-}
{-# INLINE encodeValue0# #-}
    
-- | A closure (used to implement primitive operations in the VM) is a function which takes the current data stack
--   and operates on it.
type Closure :: Type

type Closure = DataStack# RealWorld -> State# RealWorld -> (# State# RealWorld, DataStack# RealWorld #)
