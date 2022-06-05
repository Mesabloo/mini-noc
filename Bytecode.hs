{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ < 930
{-# LANGUAGE DataKinds #-}
#endif

#include "MachDeps.h"

module Bytecode where

#define WORD_SIZE_IN_BYTES (WORD_SIZE_IN_BITS# `quotInt#` 8# )

#define VALUE_SIZE_IN_BYTES (1# +# 4# )

import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (Array#, ByteArray#, Int (I#), Int#, RealWorld, State#, Word32#, indexArray#, indexIntArray#, indexWord32Array#, quotInt#, remInt#, sizeofArray#, sizeofByteArray#, (+#), (>=#))
import GHC.IO (unIO)
import GHC.Show (show)
import GHC.Types (RuntimeRep (Word32Rep), TYPE, Type, UnliftedType)
import GHC.Word (Word32 (W32#))
import Numeric (showHex)
import Runtime.Value (decodeValue1#, showValue#)
import System.IO (putStr, putStrLn)

-- | Holds 'Value#'s internally.
type ConstantTable# :: UnliftedType
type ConstantTable# = ByteArray#

type SymbolTable# :: UnliftedType
type SymbolTable# = Array# Text

-- | Holds 'Int#'s internally.
type FunctionTable# :: UnliftedType
type FunctionTable# = ByteArray#

-- | Holds 'Word32#'s internally.
type CodeTable# :: UnliftedType
type CodeTable# = ByteArray#

-- | The layout of a bytecode memory file.
type BytecodeFile :: Type
data BytecodeFile
  = File
      ConstantTable#
      -- ^ Constants
      SymbolTable#
      -- ^ Symbols
      FunctionTable#
      -- ^ Function offsets, mapping byte offsets (as integers) into the code.
      CodeTable#
      -- ^ The compiled code itself
      Int#
      -- ^ The starting instruction pointer

printBytecodeFile :: BytecodeFile -> State# RealWorld -> (# State# RealWorld, (# #) #)
printBytecodeFile (File cstTable symTable funTable codeTable ip) s0 =
  let !cstTableSize = sizeofByteArray# cstTable `quotInt#` VALUE_SIZE_IN_BYTES
      !symTableSize = sizeofArray# symTable
      !funTableSize = sizeofByteArray# funTable `quotInt#` WORD_SIZE_IN_BYTES
      !codeTableSize = sizeofByteArray# codeTable `quotInt#` 4#

      !(# s1, _ #) = unIO (putStrLn "-----| CONSTANTS |-----") s0
      !(# s2, _ #) = printConstantTable cstTable cstTableSize s1
      !(# s3, _ #) = unIO (putStrLn "-----| SYMBOLS |-----") s2
      !(# s4, _ #) = printSymbolTable symTable symTableSize s3
      !(# s5, _ #) = unIO (putStrLn "-----| FUNCTIONS |-----") s4
      !(# s6, _ #) = printFunctionTable funTable funTableSize s5
      !(# s7, _ #) = unIO (putStr "-----| CODE |-----") s6
      !(# s8, _ #) = printCodeTable codeTable codeTableSize s7
      !(# s9, _ #) = unIO (putStrLn $ "IP=" <> show (I# ip) <> ", END=" <> show (I# codeTableSize)) s8
   in (# s9, (# #) #)
{-# NOINLINE printBytecodeFile #-}

printConstantTable :: ConstantTable# -> Int# -> State# RealWorld -> (# State# RealWorld, (# #) #)
printConstantTable table size s0 = go 0# s0
  where
    go x s0 = case x >=# size of
      1# -> (# s0, (# #) #)
      _ ->
        let !(# s1, val #) = decodeValue1# table x s0
            !(# s2, !_ #) = unIO (putStrLn $ show (I# x) <> ":\t" <> showValue# val) s1
         in go (x +# 1#) s2
    {-# NOINLINE go #-}
{-# INLINE printConstantTable #-}

printSymbolTable :: SymbolTable# -> Int# -> State# RealWorld -> (# State# RealWorld, (# #) #)
printSymbolTable table size s0 = go 0# s0
  where
    go x s0 = case x >=# size of
      1# -> (# s0, (# #) #)
      _ ->
        let (# sym #) = indexArray# table x
            !(# s1, !_ #) = unIO (putStrLn $ show (I# x) <> ":\t" <> show (Text.unpack sym)) s0
         in go (x +# 1#) s1
    {-# NOINLINE go #-}
{-# INLINE printSymbolTable #-}

printFunctionTable :: FunctionTable# -> Int# -> State# RealWorld -> (# State# RealWorld, (# #) #)
printFunctionTable table size s0 = go 0# s0
  where
    go x s0 = case x >=# size of
      1# -> (# s0, (# #) #)
      _ ->
        let !off = indexIntArray# table x
            !(# s1, !_ #) = unIO (putStrLn $ show (I# x) <> ":\t+" <> show (I# off)) s0
         in go (x +# 1#) s1
    {-# NOINLINE go #-}
{-# INLINE printFunctionTable #-}

printCodeTable :: CodeTable# -> Int# -> State# RealWorld -> (# State# RealWorld, (# #) #)
printCodeTable table size s0 = go 0# s0
  where
    go x s0 = case x >=# size of
      1# ->
        let !(# s1, _ #) = unIO (putStrLn "") s0
         in (# s1, (# #) #)
      _ ->
        let !code = indexWord32Array# table x
            !(# s1, !_ #) = case remInt# x 8# of
              0# -> unIO (putStr $ "\n" <> show (I# x) <> ":\t") s0
              _ -> (# s0, () #)
            !(# s2, !_ #) = unIO (putStr $ showHex (W32# code) "\t") s1
         in go (x +# 1#) s2
    {-# NOINLINE go #-}
{-# INLINE printCodeTable #-}

------------------------------------------------------------------------------------

type ByteCode :: Type
type ByteCode = Word32

type ByteCode# :: TYPE 'Word32Rep
type ByteCode# = Word32#
