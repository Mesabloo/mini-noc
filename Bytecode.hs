{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bytecode where

import Data.Function (($))
import Data.Int (Int)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Err (undefined)
import GHC.Exts (Array#, ByteArray#, Int (I#), Int#, RealWorld, State#, Word32#, indexArray#, indexWord32Array#, quotInt#, remInt#, sizeofArray#, sizeofByteArray#, (+#), (>=#))
import GHC.IO (unIO)
import GHC.Show (show)
import GHC.Types (RuntimeRep (Word32Rep), UnliftedType, Type, TYPE)
import GHC.Word (Word32 (W32#))
import Numeric (showHex)
import Runtime.Value (Value#, showValue#)
import System.IO (putStr, putStrLn)

type ConstantTable# :: UnliftedType

type ConstantTable# = Array# Value#

type SymbolTable# :: UnliftedType

type SymbolTable# = Array# Text

type FunctionTable# :: UnliftedType

type FunctionTable# = Array# Int -- NOTE: this really has to be a ByteArray# with custom accessors

type CodeTable# :: UnliftedType

type CodeTable# = ByteArray# -- NOTE: this really needs to be a ByteArray# with custom accessors

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
  let !cstTableSize = sizeofArray# cstTable
      !symTableSize = sizeofArray# symTable
      !funTableSize = sizeofArray# funTable
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
   in (# s9, (##) #)

printConstantTable :: ConstantTable# -> Int# -> State# RealWorld -> (# State# RealWorld, (# #) #)
printConstantTable table size s0 = go 0# s0
  where
    go x s0 = case x >=# size of
      1# -> (# s0, (##) #)
      0# ->
        let (# val #) = indexArray# table x
            !(# s1, !_ #) = unIO (putStrLn $ show (I# x) <> ":\t" <> showValue# val) s0
         in go (x +# 1#) s1
      _ -> undefined

printSymbolTable :: SymbolTable# -> Int# -> State# RealWorld -> (# State# RealWorld, (# #) #)
printSymbolTable table size s0 = go 0# s0
  where
    go x s0 = case x >=# size of
      1# -> (# s0, (##) #)
      0# ->
        let (# sym #) = indexArray# table x
            !(# s1, !_ #) = unIO (putStrLn $ show (I# x) <> ":\t" <> show (Text.unpack sym)) s0
         in go (x +# 1#) s1
      _ -> undefined

printFunctionTable :: FunctionTable# -> Int# -> State# RealWorld -> (# State# RealWorld, (# #) #)
printFunctionTable table size s0 = go 0# s0
  where
    go x s0 = case x >=# size of
      1# -> (# s0, (##) #)
      0# ->
        let (# off #) = indexArray# table x
            !(# s1, !_ #) = unIO (putStrLn $ show (I# x) <> ":\t+" <> show off) s0
         in go (x +# 1#) s1
      _ -> undefined

printCodeTable :: CodeTable# -> Int# -> State# RealWorld -> (# State# RealWorld, (# #) #)
printCodeTable table size s0 = go 0# s0
  where
    go x s0 = case x >=# size of
      1# ->
        let !(# s1, _ #) = unIO (putStrLn "") s0
         in (# s1, (##) #)
      0# ->
        let !code = indexWord32Array# table x
            !(# s1, !_ #) = case remInt# x 8# of
              0# -> unIO (putStr $ "\n" <> show (I# x) <> ":\t") s0
              _ -> (# s0, () #)
            !(# s2, !_ #) = unIO (putStr $ showHex (W32# code) "\t") s1
         in go (x +# 1#) s2
      _ -> undefined

------------------------------------------------------------------------------------

type ByteCode :: Type

type ByteCode = Word32

type ByteCode# :: TYPE 'Word32Rep

type ByteCode# = Word32#
