{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

#include "./Bytecode.h"

import Bytecode (BytecodeFile (File), CodeTable#, ConstantTable#, FunctionTable#, SymbolTable#, printBytecodeFile)
import Compiler (compile)
import Control.Exception (SomeException, fromException, toException)
import Data.Bool (Bool (..), not)
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text)
import Expr (Atom (..), Expr)
import GHC.Exts (Array#, Double (..), Double#, Int (..), Int#, RealWorld, State#, TYPE, catch#, indexArray#, indexIntArray#, indexWord32Array#, int32ToInt#, quotInt#, raise#, sizeofArray#, sizeofByteArray#, word32ToInt32#, word32ToWord#, (*##), (+#), (-#), (<##), (>#), (>=#))
import GHC.IO (IO (..), unIO, unsafePerformIO)
import GHC.Types (RuntimeRep (TupleRep), Type, UnliftedRep)
#if DEBUG == 1
import GHC.Word (Word32 (W32#))
#endif
import Primitives (TypeError (TypeError))
import Runtime.Stack (CallStack#, DataStack#, StackUnderflow, freezeDataStack#, newCallStack#, newDataStack#, popCallStack#, popDataStack#, pushCallStack#, pushDataStack#)
#if DEBUG == 1
import Runtime.Stack (debugCallStack#)
#endif
import Runtime.Value (Value# (VPrimitive#, VQuote#), showValue#)
import System.CPUTime (getCPUTime)
import System.IO (print, putStr, putStrLn)
import Prelude (Integer, Show (..), fromIntegral, undefined, ($), ($!), (*), (-))

-- | The evaluation context contains the current stacks, instruction pointer and bytecode tables.
type Context :: Type
data Context
  = Context
      (DataStack# RealWorld)
      (CallStack# RealWorld)
      Int#
      ConstantTable#
      SymbolTable#
      FunctionTable#
      CodeTable#

------------------------------------------------------------------
------------- SOME EXAMPLES BECAUSE WHY NOT ----------------------
------------------------------------------------------------------

example1 :: Expr
example1 = [AIdentifier "pop"]

example2 :: Expr
example2 = [AInteger 5, AInteger 6, AIdentifier "+"]

example3 :: Expr
example3 = example2 <> example2 <> [AIdentifier "+"]

-- | Computation time:
--
--   [Original interpreter] ~6-7us
--   [Optimized VM] ~2.35us
example4 :: Expr
example4 = example3 <> example3 <> [AIdentifier "pop", AIdentifier "dup", AIdentifier "+"]

-- | Computation time:
--
--   [Original interpreter] ~150ms
--   [Optimized VM] ~30ms
example5 :: Expr
example5 = [AInteger 3, AInteger 6, AIdentifier "ack"]

-- | Computation time:
--
--   [Original interpreter] ~26us
--   [Optimized VM] ~5.2µs
example6 :: Expr
example6 = [AInteger 15, AIdentifier "fact"]

example7 :: Expr
example7 = [AInteger 15, AInteger 6, AInteger 25, AInteger 15]

example8 :: Expr
example8 = [AQuote [AInteger 1], AInteger 2]

example9 :: Expr
example9 = example8 <> [AIdentifier "pop", AIdentifier "unquote"]

example10 :: Expr
example10 = [AQuote [AQuote [AInteger 10], AIdentifier "unquote"], AIdentifier "unquote"]

example11 :: Expr
example11 = [AQuote [AInteger 10, AQuote [AInteger 12], AIdentifier "unquote"], AIdentifier "unquote"]

-- | Computation time:
--
--   [Original interpreter] ???
--   [Optimized VM] ~117ms
example12 :: Expr
example12 = [AInteger 28, AIdentifier "fib"]

------------------------------------------------------------------

showTime :: Double# -> String
showTime = go "s"
  where
    next :: String -> String
    next "s" = "ms"
    next "ms" = "us"
    next "us" = "ns"
    next _ = "ns"
    {-# INLINE next #-}

    lastUnit :: String -> Bool
    lastUnit "ns" = True
    lastUnit _ = False
    {-# INLINE lastUnit #-}

    go :: String -> Double# -> String
    go unit d = case d <## 1.0## of
      1# | not (lastUnit unit) -> go (next unit) (d *## 1000.0##)
      _ -> show (D# d) <> unit

-------------------------------------------------------------
----------------------- ENTRY POINT -------------------------
-------------------------------------------------------------

main :: IO ()
main = IO (catch# main' rethrow)
  where
    rethrow :: a -> State# RealWorld -> (# State# RealWorld, () #)
    rethrow exn s0 = (# s0, raise# exn #)

main' :: State# RealWorld -> (# State# RealWorld, () #)
main' s0 =
  let !expr = example12
      !(# s1, _ #) = unIO (putStr "> ") s0
      !(# s2, _ #) = unIO (print expr) s1
      !(# s3, !bytecodeFile0 #) = compile expr withBindings s2
      !(# s4, _ #) = printBytecodeFile bytecodeFile0 s3

      !(# s5, !dataStack #) = newDataStack# s4
      !(# s6, !callStack #) = newCallStack# s5

      !(# s7, !(# !time, (# !dataStack0, !callStack0 #) #) #) = timeItT (eval' dataStack callStack bytecodeFile0) s6

      !(# s8, _ #) = printResult time dataStack0 callStack0 s7
   in (# s8, () #)
  where
    withBindings :: [(Text, Expr)]
    withBindings =
      [ ("fact", fact),
        ("ack", ack),
        ("fib", fib)
      ]
    {-# INLINE withBindings #-}

    eval' :: DataStack# RealWorld -> CallStack# RealWorld -> BytecodeFile -> State# RealWorld -> (# State# RealWorld, (# DataStack# RealWorld, CallStack# RealWorld #) #)
    eval' stack cstack (File constants symbols functions code ip) s0 =
      eval (Context stack cstack ip constants symbols functions code) s0
    {-# INLINE eval' #-}

    timeItT :: (State# RealWorld -> (# State# RealWorld, (# DataStack# RealWorld, CallStack# RealWorld #) #)) -> State# RealWorld -> (# State# RealWorld, (# Double#, (# DataStack# RealWorld, CallStack# RealWorld #) #) #)
    timeItT f s0 =
      let !(# s1, start #) = unIO getCPUTime s0
          !(# s2, !res #) = f s1
          !(# s3, end #) = unIO getCPUTime s2
          !(D# time) = fromIntegral @Integer @Double (end - start) * 1e-12
       in (# s3, (# time, res #) #)
    {-# INLINE timeItT #-}

    printResult :: Double# -> DataStack# RealWorld -> CallStack# RealWorld -> State# RealWorld -> (# State# RealWorld, (# #) #)
    printResult time dataStack0 callStack0 s0 =
      let !(# s1, _ #) = unIO (putStr "\nresult: ") s0

          !(# s2, !arr0 #) = freezeDataStack# dataStack0 s1
          !(# s3, !_ #) = printArrayBounds 0# (sizeofArray# arr0 -# 1#) arr0 s2
          !(# s4, _ #) = unIO (putStrLn $ "time taken: " <> showTime time) s3
       in (# s4, (# #) #)
    {-# INLINE printResult #-}
{-# INLINE main' #-}

-- | Print all the elements of the given array within the specified bounds.
printArrayBounds :: Int# -> Int# -> Array# Value# -> State# RealWorld -> (# State# RealWorld, () #)
printArrayBounds low high arr s0 = go low s0
  where
    go x s0 =
      case x ># high of
        1# ->
          let !_ = unsafePerformIO (putStrLn "")
           in (# s0, () #)
        0# ->
          let !(# s1, _ #) = printIthOfArray arr x s0
           in go (x +# 1#) s1
        _ -> undefined

    printIthOfArray :: Array# Value# -> Int# -> State# RealWorld -> (# State# RealWorld, () #)
    printIthOfArray arr i s0 =
      let (# !val #) = indexArray# arr i
       in let !_ = unsafePerformIO (putStr $! showValue# val <> " ")
           in (# s0, () #)
    {-# INLINE printIthOfArray #-}
{-# INLINE printArrayBounds #-}

-------------------------------------------------------------
------------------ INTERPRETER ENTRY POINT ------------------
-------------------------------------------------------------

{- ORMOLU_DISABLE -}

type Lift :: TYPE ('TupleRep '[UnliftedRep, UnliftedRep]) -> Type
data Lift a = Lift a

eval :: Context -> State# RealWorld -> (# State# RealWorld, (# DataStack# RealWorld, CallStack# RealWorld #) #)
eval (Context dataStack callStack ip constants _ functions code) s0 =
  let !codeSize = sizeofByteArray# code `quotInt#` 4#
      !(# s1, (Lift !dataStack0, Lift !callStack0) #) = catch# (go dataStack callStack codeSize ip) handler s0
   in (# s1, (# dataStack0, callStack0 #) #)
  where
    go :: DataStack# RealWorld -> CallStack# RealWorld -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, (Lift (DataStack# RealWorld), Lift (CallStack# RealWorld)) #)
    go dataStack callStack size ip0 s0 =
      case ip0 >=# size of
        1# -> (# s0, (Lift dataStack, Lift callStack) #)
        _ ->
#if DEBUG == 1
          let !_ = unsafePerformIO (putStrLn $ "code size=(expected=" <> show (I# size) <> ", real=" <> show (I# (sizeofByteArray# code `quotInt#` 4#)) <> "), access at=" <> show (I# ip0))
              _ = debugCallStack# callStack s0
           in
#endif
          case word32ToWord# (indexWord32Array# code ip0) of
            BYTECODE_RET## ->
              let !(# s1, !off #) = popCallStack# callStack s0
#if DEBUG == 1
                  !_ = unsafePerformIO (putStrLn $ "> Returning to address " <> show (I# off))
#endif
               in go dataStack callStack size off s1
            BYTECODE_PRIM## ->
#if DEBUG == 1
              let !_ = unsafePerformIO (putStrLn $ ">> PRIM: get index from offset " <> show (I# (ip0 +# 1#))) in
#endif
              let !idx = indexWord32Array# code (ip0 +# 1#)
                  (# VPrimitive# !f _ #) = indexArray# constants (int32ToInt# (word32ToInt32# idx))
#if DEBUG == 1
                  !_ = unsafePerformIO (putStrLn $ "> Computing primitive at index " <> show (W32# idx))
#endif
                  !(# s1, !stack0 #) = f dataStack s0
               in go stack0 callStack size (ip0 +# 2#) s1
            BYTECODE_PUSH## ->
              let !idx = indexWord32Array# code (ip0 +# 1#)
                  (# !cst #) = indexArray# constants (int32ToInt# (word32ToInt32# idx))
                  !(# s1, !stack0 #) = pushDataStack# dataStack cst s0
#if DEBUG == 1
                  !_ = unsafePerformIO (putStrLn $ "> Pushing constant #" <> show (W32# idx) <> " (" <> showValue# cst <> ")")
#endif
               in go stack0 callStack size (ip0 +# 2#) s1
            BYTECODE_JUMP## ->
              let !idx = indexWord32Array# code (ip0 +# 1#)
                  !off = indexIntArray# functions (int32ToInt# (word32ToInt32# idx))
                  !(# s1, !stack0 #) = pushCallStack# callStack (ip0 +# 2#) s0
#if DEBUG == 1
                  !_ = unsafePerformIO (putStrLn $ "> Jumping to code offset " <> show (I# off) <> " found at entry #" <> show (W32# idx) <> " from ip=" <> show (I# ip0))
#endif
               in go dataStack stack0 size off s1
            BYTECODE_UNQUOTE## ->
              let !(# s1, !val #) = popDataStack# dataStack s0 in
              case val of
                VQuote# off ->
                  let !(# s2, !stack0 #) = pushCallStack# callStack (ip0 +# 1#) s1
#if DEBUG == 1
                      !_ = unsafePerformIO (putStrLn $ "> Unquotting quote found at offset " <> show (I# off) <> " from ip=" <> show (I# ip0))
#endif
                   in go dataStack stack0 size off s2
                val -> raise# (toException $ TypeError $ "Not a quote: " <> showValue# val)
            _ -> undefined

    handler :: SomeException -> State# RealWorld -> (# State# RealWorld, (Lift (DataStack# RealWorld), Lift (CallStack# RealWorld)) #)
    handler (!exn :: SomeException) s0 =
      let !(# s1, () #) = case fromException @StackUnderflow exn of
            Just !_ -> unIO (putStrLn "\n[!] Tried popping a value off an empty stack.") s0
            Nothing -> (# s0, () #)

          !(# s2, () #) = case fromException @TypeError exn of
            Just !(TypeError msg) -> unIO (putStrLn $ "\n[!] Type error on evaluation: " <> msg) s1
            Nothing -> (# s1, () #)

       in (# s2, raise# exn #)
    {-# INLINE handler #-}
{-# INLINE eval #-}
{-# SCC eval "evaluation" #-}

{- ORMOLU_ENABLE -}

---------------------------------------------------------------------------
-------------------------------- FOR TESTS --------------------------------
---------------------------------------------------------------------------

fact :: Expr
fact =
  [ -- [n]
    AIdentifier "dup", -- [n n]
    AInteger 0, -- [n n 0]
    AIdentifier "=", -- [n (n=0)]
    AQuote
      [ -- > [n]
        AIdentifier "pop", -- > []
        AInteger 1 -- > [1]
      ], -- [n (n=0) [1]]
    AQuote
      [ -- > [n]
        AIdentifier "dup", -- > [n n]
        AInteger 1, -- > [n n 1]
        AIdentifier "-", -- > [n (n-1)]
        AIdentifier "fact", -- > [n (n-1) fact]
        -- AIdentifier "unquote", -- > [n (fact(n-1))]
        AIdentifier "*" -- > [(n*fact(n-1))]
      ], -- [n (n=0) [1] [(n*fact(n-1))]]
    AIdentifier "if", -- [n [1] | [(n*fact(n-1))]]
    AIdentifier "unquote" -- [1 | n*fact(n-1)]
  ]

ack :: Expr
ack =
  [ -- [m n]
    AIdentifier "swap", -- [n m]
    AIdentifier "dup", -- [n m m]
    AInteger 0, -- [n m m 0]
    AIdentifier "=", -- [n m (m=0)]
    AQuote
      [ -- > [n m]
        AIdentifier "pop", -- > [n]
        AInteger 1, -- > [n 1]
        AIdentifier "+" -- > [(n+1)]
      ], -- [n m (m=0) [(n+1)]]
    AQuote
      [ -- > [n m]
        AIdentifier "swap", -- > [m n]
        AIdentifier "dup", -- > [m n n]
        AInteger 0, -- > [m n n 0]
        AIdentifier "=", -- > [m n (n=0)]
        AQuote
          [ -- >> [m n]
            AIdentifier "pop", -- >> [m]
            AInteger 1, -- >> [m 1]
            AIdentifier "-", -- >> [(m-1)]
            AInteger 1, -- >> [(m-1) 1)]
            AIdentifier "ack" -- >> [(m-1) 1 ack]
          ], -- > [m n (n=0) [(ack(m-1,1))]]
        AQuote
          [ -- >> [m n]
            AIdentifier "swap", -- >> [n m]
            AIdentifier "dup", -- >> [n m m]
            AIdentifier "rot31", -- >> [m n m]
            AInteger 1, -- >> [m n m 1]
            AIdentifier "-", -- >> [m n (m-1)]
            AIdentifier "rot31", -- >> [(m-1) m n]
            AInteger 1, -- >> [(m-1) m n 1]
            AIdentifier "-", -- >> [(m-1) m (n-1)]
            AIdentifier "ack", -- >> [(m-1) (ack(m,n-1))]
            AIdentifier "ack" -- >> [(ack(m-1,ack(m,n-1)))]
          ], -- > [m n (n=0) [ack(m-1,1)] [ack(m-1,ack(m,n-1))]]
        AIdentifier "if", -- > [m n ([ack(m-1,1)] | [ack(m-1,ack(m,n-1))])]
        AIdentifier "unquote" -- > [(ack(m-1,1) | ack(m-1,ack(m,n-1)))]
      ], -- [n m (m=0) [(n+1)] [(ack(m-1,1) | ack(m-1,ack(m,n-1)))]]
    AIdentifier "if", -- [n m [(n+1) | (ack(m-1,1) | ack(m-1,ack(m,n-1)))]]
    AIdentifier "unquote" -- [(n+1 | ack(m-1,1) | ack(m-1,ack(m,n-1)))]
  ]

fib :: Expr
fib =
  [ -- [n]
    AIdentifier "dup", -- [n n]
    AInteger 2, -- [n n 2]
    AIdentifier "<", -- [n (n<2)]
    AQuote -- > [n]
      [],
    -- [n (n<2) [n]]
    AQuote
      [ -- > [n]
        AIdentifier "dup", -- > [n n]
        AInteger 1, -- > [n n 1]
        AIdentifier "-", -- > [n (n-1)]
        AIdentifier "fib", -- > [n fib(n-1)]
        AIdentifier "swap", -- > [fib(n-1) n]
        AInteger 2, -- > [fib(n-1) n 2]
        AIdentifier "-", -- > [fib(n-1) (n-2)]
        AIdentifier "fib", -- > [fib(n-1) fib(n-2)]
        AIdentifier "+" -- > [fib(n-1)+fib(n-2)]
      ], -- [n (n<2) [n] [fib(n-1)+fib(n-2)]
    AIdentifier "if", -- [[n] | [fib(n-1)+fib(n-2)]]
    AIdentifier "unquote" -- [n | fib(n-1)+fib(n-2)]
  ]
