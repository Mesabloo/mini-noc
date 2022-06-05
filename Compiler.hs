{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}

#include "./Bytecode.h"
#include "./Primitives.h"
#include "MachDeps.h"

#define WORD_SIZE_IN_BYTES (WORD_SIZE_IN_BITS# `quotInt#` 8# )

#define VALUE_SIZE_IN_BYTES (1# +# 4# )

module Compiler (compile, UnboundIdentifier (..)) where

import Bytecode (ByteCode, BytecodeFile (File))
import Data.Eq ((==))
import Data.List qualified as List
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text)
import Data.Text qualified as Text
import Expr (Atom (..), Expr)
import GHC.Base (($!))
import GHC.Err (undefined)
import GHC.Exception (Exception)
import GHC.Exts (Array#, Char (C#), Float (F#), Int (I#), Int#, MutVar#, MutableArray#, MutableByteArray#, RealWorld, State#, copyMutableArray#, copyMutableByteArray#, freezeArray#, getSizeofMutableByteArray#, indexArray#, int32ToWord32#, intToInt32#, newArray#, newByteArray#, newMutVar#, newPinnedByteArray#, quotInt#, raise#, readMutVar#, resizeMutableByteArray#, sizeofArray#, sizeofMutableArray#, unsafeFreezeArray#, unsafeFreezeByteArray#, writeArray#, writeIntArray#, writeMutVar#, writeWord32Array#, (*#), (+#), (>=#))
import GHC.Num ((+))
import GHC.Show (Show, show)
import GHC.Types (Type)
import GHC.Word (Word32 (W32#))
import Runtime.Value (Bool# (..), Value# (..), decodeValue0#, encodeValue0#, eqValue#)
import Text.Read (read)

type UnboundIdentifier :: Type
newtype UnboundIdentifier = UnboundIdentifier String
  deriving stock (Show)

instance Exception UnboundIdentifier

resizeConstantTableIfNeeded :: Int# -> MutableByteArray# RealWorld -> State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld #)
resizeConstantTableIfNeeded 0# constants s0 = (# s0, constants #)
resizeConstantTableIfNeeded 1# constants s0 =
  let !(# s1, constantsSize #) = getSizeofMutableByteArray# constants s0
      -- !_ = unsafePerformIO (putStrLn $ ">> Resizing constant table from " <> show (I# constantsSize) <> " to " <> show (I# (constantsSize *# 2#))) in
      -- !(# s1, arr #) = newArray# (constantsSize *# 2#) (VInteger# 0#) s0
      -- !s2 = copyMutableArray# constants 0# arr 0# constantsSize s1
      !(# s2, !arr0 #) = resizeMutableByteArray# constants (constantsSize *# 2#) s1
   in (# s2, arr0 #)
resizeConstantTableIfNeeded _ _ _ = undefined

resizeSymbolsTableIfNeeded :: Int# -> MutableArray# RealWorld Text -> State# RealWorld -> (# State# RealWorld, MutableArray# RealWorld Text #)
resizeSymbolsTableIfNeeded 0# symbols s0 = (# s0, symbols #)
resizeSymbolsTableIfNeeded 1# symbols s0 =
  let !symbolsSize = sizeofMutableArray# symbols
      -- !_ = unsafePerformIO (putStrLn $ ">> Resizing symbols table from " <> show (I# symbolsSize) <> " to " <> show (I# (symbolsSize *# 2#))) in
      !(# s1, arr #) = newArray# (symbolsSize *# 2#) ("" :: Text) s0
      !s2 = copyMutableArray# symbols 0# arr 0# symbolsSize s1
   in (# s2, arr #)
resizeSymbolsTableIfNeeded _ _ _ = undefined

resizeFunctionsTableIfNeeded :: Int# -> MutableByteArray# RealWorld -> State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld #)
resizeFunctionsTableIfNeeded 0# functions s0 = (# s0, functions #)
resizeFunctionsTableIfNeeded 1# functions s0 =
  let !(# s1, functionsSize #) = getSizeofMutableByteArray# functions s0
      -- !_ = unsafePerformIO (putStrLn $ ">> Resizing functions table from " <> show (I# functionsSize) <> " to " <> show (I# (functionsSize *# 2#))) in
      -- !(# s2, arr #) = newArray# (functionsSize *# 2#) (0 :: Int) s0
      -- !s2 = copyMutableArray# functions 0# arr 0# functionsSize s1
      !(# s2, !arr0 #) = resizeMutableByteArray# functions (functionsSize *# 2#) s1
   in (# s2, arr0 #)
resizeFunctionsTableIfNeeded _ _ _ = undefined

resizeCodeTableIfNeeded :: Int# -> MutableByteArray# RealWorld -> State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld #)
resizeCodeTableIfNeeded 0# code s0 = (# s0, code #)
resizeCodeTableIfNeeded 1# code s0 =
  let !(# s1, codeSize #) = getSizeofMutableByteArray# code s0
      -- !_ = unsafePerformIO (putStrLn $ ">> Resizing code table from " <> show (I# codeSize) <> "B to " <> show (I# (codeSize *# 2#)) <> "B")
      !(# s2, !arr0 #) = resizeMutableByteArray# code (codeSize *# 2#) s1
   in -- !(# s2, arr #) = newByteArray# (codeSize *# 2#) s1
      -- !s3 = copyMutableByteArray# code 0# arr 0# codeSize s2
      (# s2, arr0 #)
resizeCodeTableIfNeeded _ _ _ = undefined

-- | Compile a surface language expression into an in-memory BYTECODE_ file ready to be evaluated.
compile :: Expr -> [(Text, Expr)] -> State# RealWorld -> (# State# RealWorld, BytecodeFile #)
compile expr bindings s0 =
  let !(# s1, (# constants, constantsPtr #) #) = createConstantArray s0
      !(# s2, (# symbols, symbolsPtr #) #) = createSymbolsArray s1
      !(# s3, (# functions, functionsPtr #) #) = createFunctions s2
      !(# s4, (# code, codePtr #) #) = createCode s3

      !(# s5, (# ip, constants0, symbols0, functions0, code0 #) #) =
        go expr bindings constants constantsPtr symbols symbolsPtr functions functionsPtr code codePtr s4

      !(# s6, I# constantsPtr0 #) = readMutVar# constantsPtr s5
      !(# s7, constants1 #) = newPinnedByteArray# (constantsPtr0 *# VALUE_SIZE_IN_BYTES) s6
      !s8 = copyMutableByteArray# constants0 0# constants1 0# (constantsPtr0 *# VALUE_SIZE_IN_BYTES) s7
      !(# s9, !constants2 #) = unsafeFreezeByteArray# constants1 s8

      !(# s10, I# symbolsPtr0 #) = readMutVar# symbolsPtr s9
      !(# s11, !symbols1 #) = freezeArray# symbols0 0# symbolsPtr0 s10

      !(# s12, I# functionsPtr0 #) = readMutVar# functionsPtr s11
      !(# s13, functions1 #) = newPinnedByteArray# (functionsPtr0 *# WORD_SIZE_IN_BYTES) s12
      !s14 = copyMutableByteArray# functions0 0# functions1 0# (functionsPtr0 *# WORD_SIZE_IN_BYTES) s13
      !(# s15, !functions2 #) = unsafeFreezeByteArray# functions1 s14
      -- !(# s11, functions1 #) = freezeArray# functions0 0# functionsPtr0 s10

      !(# s16, I# codePtr0 #) = readMutVar# codePtr s15
      -- !s13 = shrinkMutableByteArray# code0 (codePtr0 *# 4#) s12
      !(# s17, code1 #) = newPinnedByteArray# (codePtr0 *# 4#) s16
      !s18 = copyMutableByteArray# code0 0# code1 0# (codePtr0 *# 4#) s17
      !(# s19, !code2 #) = unsafeFreezeByteArray# code1 s18
   in -- NOTE: symbolsPtr0 ==# functionsPtr0 must be 1#

      (# s19, File constants2 symbols1 functions2 code2 ip #)
  where
    createConstantArray :: State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, MutVar# RealWorld Int #) #)
    createConstantArray s0 =
      let !(# s1, constants #) = newByteArray# (10# *# VALUE_SIZE_IN_BYTES) s0
          !(# s2, ptr #) = newMutVar# (0 :: Int) s1
       in (# s2, (# constants, ptr #) #)

    createSymbolsArray :: State# RealWorld -> (# State# RealWorld, (# MutableArray# RealWorld Text, MutVar# RealWorld Int #) #)
    createSymbolsArray s0 =
      let !(# s1, symbols #) = newArray# 10# ("" :: Text) s0
          !(# s2, ptr #) = newMutVar# (0 :: Int) s1
       in (# s2, (# symbols, ptr #) #)

    createFunctions :: State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, MutVar# RealWorld Int #) #)
    createFunctions s0 =
      let !(# s1, functions #) = newByteArray# (10# *# WORD_SIZE_IN_BYTES) s0
          !(# s2, functionsPtr #) = newMutVar# (0 :: Int) s1
       in (# s2, (# functions, functionsPtr #) #)

    createCode :: State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, MutVar# RealWorld Int #) #)
    createCode s0 =
      let !(# s1, code #) = newByteArray# 20# s0
          !(# s2, codePtr #) = newMutVar# (0 :: Int) s1
       in (# s2, (# code, codePtr #) #)

    go expr bindings constants constantsPtr symbols symbolsPtr functions functionsPtr code codePtr s0 =
      let !(# s1, (# constants0, symbols0, functions0, code0 #) #) =
            compileAdditionalBindings bindings constants constantsPtr symbols symbolsPtr functions functionsPtr code codePtr s0
       in compileExpr expr constants0 constantsPtr symbols0 symbolsPtr functions0 functionsPtr code0 codePtr s1

    compileAdditionalBindings ::
      [(Text, Expr)] ->
      MutableByteArray# RealWorld ->
      MutVar# RealWorld Int ->
      MutableArray# RealWorld Text ->
      MutVar# RealWorld Int ->
      MutableByteArray# RealWorld ->
      MutVar# RealWorld Int ->
      MutableByteArray# RealWorld ->
      MutVar# RealWorld Int ->
      State# RealWorld ->
      (# State# RealWorld, (# MutableByteArray# RealWorld, MutableArray# RealWorld Text, MutableByteArray# RealWorld, MutableByteArray# RealWorld #) #)
    compileAdditionalBindings [] constants _ symbols _ functions _ code _ s0 = (# s0, (# constants, symbols, functions, code #) #)
    compileAdditionalBindings ((name, val) : binds) constants constantsPtr symbols symbolsPtr functions functionsPtr code codePtr s0 =
      let !(# s1, !(I# codePtr0) #) = readMutVar# codePtr s0
          !(# s2, !(I# symbolsPtr0) #) = readMutVar# symbolsPtr s1
          !(# s3, !(I# functionsPtr0) #) = readMutVar# functionsPtr s2

          !symbolsSize = sizeofMutableArray# symbols
          !(# s4, functionsSize #) = getSizeofMutableByteArray# functions s3

          !(# s5, symbols0 #) = resizeSymbolsTableIfNeeded (symbolsPtr0 >=# symbolsSize) symbols s4
          !(# s6, functions0 #) = resizeFunctionsTableIfNeeded (functionsPtr0 *# WORD_SIZE_IN_BYTES >=# functionsSize) functions s5

          !s7 = writeArray# symbols0 symbolsPtr0 name s6
          !s8 = writeIntArray# functions0 functionsPtr0 codePtr0 s7

          !(# s9, (# ip, constants0, symbols1, functions1, code0 #) #) =
            compileExpr val constants constantsPtr symbols0 symbolsPtr functions0 functionsPtr code codePtr s8

          !(# s10, I# codePtr1 #) = readMutVar# codePtr s9
          !(# s11, codeSize #) = getSizeofMutableByteArray# code0 s10
          !(# s12, code1 #) = resizeCodeTableIfNeeded (codePtr1 *# 4# >=# codeSize) code0 s11
          !(# s13, code2 #) = pushOpcodes [BYTECODE_RET] codePtr1 code1 codePtr s12

          !s14 = writeIntArray# functions1 functionsPtr0 ip s13
          !s15 = writeMutVar# functionsPtr (I# (functionsPtr0 +# 1#)) s14
          !s16 = writeMutVar# symbolsPtr (I# (symbolsPtr0 +# 1#)) s15
       in compileAdditionalBindings binds constants0 constantsPtr symbols1 symbolsPtr functions1 functionsPtr code2 codePtr s16
{-# INLINEABLE compile #-}

compileExpr ::
  Expr ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  MutableArray# RealWorld Text ->
  MutVar# RealWorld Int ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  State# RealWorld ->
  (# State# RealWorld, (# Int#, MutableByteArray# RealWorld, MutableArray# RealWorld Text, MutableByteArray# RealWorld, MutableByteArray# RealWorld #) #)
compileExpr expr constants constantsPtr symbols symbolsPtr functions functionsPtr code codePtr s0 =
  let !(# s1, (# constants0, symbols0, functions0, code0, expr0 #) #) =
        precompileQuotes expr constants constantsPtr symbols symbolsPtr functions functionsPtr code codePtr s0
      !(# s2, I# ip #) = readMutVar# codePtr s1
      !(# s3, (# constants1, symbols1, functions1, code1 #) #) =
        compileAtoms expr0 constants0 constantsPtr symbols0 symbolsPtr functions0 functionsPtr code0 codePtr s2
   in (# s3, (# ip, constants1, symbols1, functions1, code1 #) #)

precompileQuotes ::
  Expr ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  MutableArray# RealWorld Text ->
  MutVar# RealWorld Int ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  State# RealWorld ->
  (# State# RealWorld, (# MutableByteArray# RealWorld, MutableArray# RealWorld Text, MutableByteArray# RealWorld, MutableByteArray# RealWorld, Expr #) #)
precompileQuotes = go []
  where
    go ::
      Expr ->
      Expr ->
      MutableByteArray# RealWorld ->
      MutVar# RealWorld Int ->
      MutableArray# RealWorld Text ->
      MutVar# RealWorld Int ->
      MutableByteArray# RealWorld ->
      MutVar# RealWorld Int ->
      MutableByteArray# RealWorld ->
      MutVar# RealWorld Int ->
      State# RealWorld ->
      (# State# RealWorld, (# MutableByteArray# RealWorld, MutableArray# RealWorld Text, MutableByteArray# RealWorld, MutableByteArray# RealWorld, Expr #) #)
    go acc [] constants _ symbols _ functions _ code _ s0 = (# s0, (# constants, symbols, functions, code, List.reverse acc #) #)
    go acc (atom : expr) constants constantsPtr symbols symbolsPtr functions functionsPtr code codePtr s0 = case atom of
      AQuote expr0 ->
        let !(# s1, (# constants0, symbols0, functions0, code0, expr1 #) #) =
              go [] expr0 constants constantsPtr symbols symbolsPtr functions functionsPtr code codePtr s0
            !(# s2, I# codePtr0 #) = readMutVar# codePtr s1
            !(# s3, (# constants1, symbols1, functions1, code1 #) #) =
              compileAtoms expr1 constants0 constantsPtr symbols0 symbolsPtr functions0 functionsPtr code0 codePtr s2
            !(# s4, I# codePtr1 #) = readMutVar# codePtr s3
            !(# s5, codeSize #) = getSizeofMutableByteArray# code1 s4
            !(# s6, code2 #) = resizeCodeTableIfNeeded (codePtr1 *# 4# >=# codeSize) code1 s5
            !(# s7, code3 #) = pushOpcodes [BYTECODE_RET] codePtr1 code2 codePtr s6
            !quoteId = ("$" <> Text.pack (show (I# codePtr0)))
         in go (AIdentifier quoteId : acc) expr constants1 constantsPtr symbols1 symbolsPtr functions1 functionsPtr code3 codePtr s7
      _ -> go (atom : acc) expr constants constantsPtr symbols symbolsPtr functions functionsPtr code codePtr s0

compileAtoms ::
  Expr ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  MutableArray# RealWorld Text ->
  MutVar# RealWorld Int ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  State# RealWorld ->
  (# State# RealWorld, (# MutableByteArray# RealWorld, MutableArray# RealWorld Text, MutableByteArray# RealWorld, MutableByteArray# RealWorld #) #)
compileAtoms [] constants _ symbols _ functions _ code _ s0 = (# s0, (# constants, symbols, functions, code #) #)
compileAtoms (atom : expr) constants constantsPtr symbols symbolsPtr functions functionsPtr code codePtr s0 =
  let !(# s1, (# constants0, symbols0, functions0, code0 #) #) = compileAtom atom constants symbols functions code s0
   in compileAtoms expr constants0 constantsPtr symbols0 symbolsPtr functions0 functionsPtr code0 codePtr s1
  where
    compileAtom ::
      Atom ->
      MutableByteArray# RealWorld ->
      MutableArray# RealWorld Text ->
      MutableByteArray# RealWorld ->
      MutableByteArray# RealWorld ->
      State# RealWorld ->
      (# State# RealWorld, (# MutableByteArray# RealWorld, MutableArray# RealWorld Text, MutableByteArray# RealWorld, MutableByteArray# RealWorld #) #)
    compileAtom (AInteger (I# i)) constants symbols functions code s0 =
      let !(# s1, (# constants0, code0 #) #) = insertConstant (VInteger# i) constants constantsPtr code codePtr s0
       in (# s1, (# constants0, symbols, functions, code0 #) #)
    compileAtom (ACharacter (C# c)) constants symbols functions code s0 =
      let !(# s1, (# constants0, code0 #) #) = insertConstant (VCharacter# c) constants constantsPtr code codePtr s0
       in (# s1, (# constants0, symbols, functions, code0 #) #)
    compileAtom (ABoolean b) constants symbols functions code s0 =
      let !(# s1, (# constants0, code0 #) #) = insertConstant (VBoolean# if b then True# else False#) constants constantsPtr code codePtr s0
       in (# s1, (# constants0, symbols, functions, code0 #) #)
    compileAtom (AFloat (F# d)) constants symbols functions code s0 =
      let !(# s1, (# constants0, code0 #) #) = insertConstant (VDouble# d) constants constantsPtr code codePtr s0
       in (# s1, (# constants0, symbols, functions, code0 #) #)
    compileAtom (AIdentifier "unquote") constants symbols functions code s0 =
      let !(# s1, I# codePtr0 #) = readMutVar# codePtr s0
          !(# s2, codeSize #) = getSizeofMutableByteArray# code s1
          !(# s3, code0 #) = resizeCodeTableIfNeeded (codePtr0 *# 4# >=# codeSize) code s2
          !(# s4, code1 #) = pushOpcodes [BYTECODE_UNQUOTE] codePtr0 code0 codePtr s3
       in (# s4, (# constants, symbols, functions, code1 #) #)
    compileAtom (AIdentifier "pop") constants symbols functions code s0 =
      let !(# s1, code0 #) = insertBuiltin PRIM_POP_IDX# code codePtr s0
       in (# s1, (# constants, symbols, functions, code0 #) #)
    compileAtom (AIdentifier "dup") constants symbols functions code s0 =
      let !(# s1, code0 #) = insertBuiltin PRIM_DUP_IDX# code codePtr s0
       in (# s1, (# constants, symbols, functions, code0 #) #)
    compileAtom (AIdentifier "swap") constants symbols functions code s0 =
      let !(# s1, code0 #) = insertBuiltin PRIM_SWAP_IDX# code codePtr s0
       in (# s1, (# constants, symbols, functions, code0 #) #)
    compileAtom (AIdentifier "if") constants symbols functions code s0 =
      let !(# s1, code0 #) = insertBuiltin PRIM_IF_IDX# code codePtr s0
       in (# s1, (# constants, symbols, functions, code0 #) #)
    compileAtom (AIdentifier "rot31") constants symbols functions code s0 =
      let !(# s1, code0 #) = insertBuiltin PRIM_ROT31_IDX# code codePtr s0
       in (# s1, (# constants, symbols, functions, code0 #) #)
    compileAtom (AIdentifier "+") constants symbols functions code s0 =
      let !(# s1, code0 #) = insertBuiltin PRIM_ADD_IDX# code codePtr s0
       in (# s1, (# constants, symbols, functions, code0 #) #)
    compileAtom (AIdentifier "-") constants symbols functions code s0 =
      let !(# s1, code0 #) = insertBuiltin PRIM_SUB_IDX# code codePtr s0
       in (# s1, (# constants, symbols, functions, code0 #) #)
    compileAtom (AIdentifier "*") constants symbols functions code s0 =
      let !(# s1, code0 #) = insertBuiltin PRIM_TIMES_IDX# code codePtr s0
       in (# s1, (# constants, symbols, functions, code0 #) #)
    compileAtom (AIdentifier "=") constants symbols functions code s0 =
      let !(# s1, code0 #) = insertBuiltin PRIM_EQ_IDX# code codePtr s0
       in (# s1, (# constants, symbols, functions, code0 #) #)
    compileAtom (AIdentifier "<") constants symbols functions code s0 =
      let !(# s1, code0 #) = insertBuiltin PRIM_LESSTHAN_IDX# code codePtr s0
       in (# s1, (# constants, symbols, functions, code0 #) #)
    compileAtom (AIdentifier name) constants symbols functions code s0 =
      case (# Text.head name, Text.tail name #) of
        (# '$', !offset #) ->
          let !(I# off) = read (Text.unpack offset)
              !(# s1, (# constants0, code0 #) #) = insertConstant (VQuote# off) constants constantsPtr code codePtr s0
           in (# s1, (# constants0, symbols, functions, code0 #) #)
        _ ->
          let !(# s1, index #) = findIndexFromReducerName symbols name s0
              !(# s2, I# codePtr0 #) = readMutVar# codePtr s1
              !(# s3, codeSize #) = getSizeofMutableByteArray# code s2
              !(# s4, code0 #) = resizeCodeTableIfNeeded ((codePtr0 +# 1#) *# 4# >=# codeSize) code s3
              !(# s5, code1 #) = pushOpcodes [BYTECODE_JUMP, W32# (int32ToWord32# (intToInt32# index))] codePtr0 code0 codePtr s4
           in (# s5, (# constants, symbols, functions, code1 #) #)
    compileAtom (AQuote _) _ _ _ _ _ = undefined

pushOpcodes :: [ByteCode] -> Int# -> MutableByteArray# RealWorld -> MutVar# RealWorld Int -> State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld #)
pushOpcodes opcodes offset code codePtr s0 =
  let !size = List.length opcodes
      !s1 = writeMutVar# codePtr (I# offset + size) s0
   in -- !_ = unsafePerformIO (putStrLn $ "Pushing " <> show size <> " opcodes " <> show opcodes <> " onto the code section starting at offset " <> show (I# offset))
      go opcodes offset s1
  where
    go [] _ s0 = (# s0, code #)
    go (W32# op : ops) off s0 = go ops (off +# 1#) (writeWord32Array# code off op s0)
{-# INLINE pushOpcodes #-}

insertConstant ::
  Value# ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  State# RealWorld ->
  (# State# RealWorld, (# MutableByteArray# RealWorld, MutableByteArray# RealWorld #) #)
insertConstant cst table cstPtr code codePtr s0 =
  let !(# s1, !(I# cstPtr0) #) = readMutVar# cstPtr s0
      !(# s2, !(I# codePtr0) #) = readMutVar# codePtr s1
      !(# s3, (# index, table0 #) #) = insertConstantInTableOnlyWhenNotFound cst table cstPtr cstPtr0 s2
      !(# s4, codeSize #) = getSizeofMutableByteArray# code s3
      !(# s5, code0 #) = resizeCodeTableIfNeeded ((codePtr0 +# 1#) *# 4# >=# codeSize) code s4
      !s6 = encodeValue0# cst table0 cstPtr0 s5
      !(# s7, code1 #) = pushOpcodes [BYTECODE_PUSH, W32# (int32ToWord32# (intToInt32# index))] codePtr0 code0 codePtr s6
   in (# s7, (# table0, code1 #) #)

insertBuiltin ::
  Int# ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  State# RealWorld ->
  (# State# RealWorld, MutableByteArray# RealWorld #)
insertBuiltin id code codePtr s0 =
  let !(# s1, !(I# codePtr0) #) = readMutVar# codePtr s0

      !(# s2, codeSize #) = getSizeofMutableByteArray# code s1
      !(# s3, code0 #) = resizeCodeTableIfNeeded ((codePtr0 +# 1#) *# 4# >=# codeSize) code s2
      !(# s4, code1 #) = pushOpcodes [BYTECODE_PRIM, W32# (int32ToWord32# (intToInt32# id))] codePtr0 code0 codePtr s3
   in (# s4, code1 #)

insertConstantInTableOnlyWhenNotFound ::
  Value# ->
  MutableByteArray# RealWorld ->
  MutVar# RealWorld Int ->
  Int# ->
  State# RealWorld ->
  (# State# RealWorld, (# Int#, MutableByteArray# RealWorld #) #)
insertConstantInTableOnlyWhenNotFound cst constants cstPtr cstPtr0 s0 = go constants 0# cstPtr0 s0
  where
    go :: MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, (# Int#, MutableByteArray# RealWorld #) #)
    go table x end s0 = case x >=# end of
      1# ->
        let !(# s1, cstSize #) = getSizeofMutableByteArray# table s0
            !(# s2, table0 #) = resizeConstantTableIfNeeded (end *# VALUE_SIZE_IN_BYTES >=# cstSize) table s1
            !s3 = writeMutVar# cstPtr (I# (end +# 1#)) s2
         in (# s3, (# end, table0 #) #)
      0# ->
        let !(# s1, value #) = decodeValue0# table x s0
         in case eqValue# cst value of
              0# -> go table (x +# 1#) end s1
              1# -> (# s1, (# x, table #) #)
              _ -> undefined
      _ -> undefined

findIndexFromReducerName :: MutableArray# RealWorld Text -> Text -> State# RealWorld -> (# State# RealWorld, Int# #)
findIndexFromReducerName functions name s0 =
  let !(# s1, functions0 #) = unsafeFreezeArray# functions s0
   in go functions0 0# (sizeofArray# functions0) s1
  where
    go :: Array# Text -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    go funs x end s0 = case x >=# end of
      1# -> raise# $! UnboundIdentifier (Text.unpack name)
      0# ->
        let (# !fnName #) = indexArray# funs x
         in if fnName == name
              then (# s0, x #)
              else go funs (x +# 1#) end s0
      _ -> undefined
