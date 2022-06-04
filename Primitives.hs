{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Primitives where

import Control.Exception (toException)
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.String (String)
import GHC.Err (undefined)
import GHC.Exception (Exception)
import GHC.Exts (raise#, (*#), (+#), (-#), (<#), (<##), (==#), (==##))
import GHC.Show (Show)
import GHC.Types (Type)
import Runtime.Stack (peekDataStack#, popDataStack#, pushDataStack#)
import Runtime.Value (Bool# (..), Closure, Value# (..), showValue#)

type TypeError :: Type
newtype TypeError = TypeError String
  deriving stock (Show)

--  deriving anyclass (Exception)

instance Exception TypeError

pop :: Closure
pop stack s0 =
  let !(# s1, _ #) = popDataStack# stack s0
   in (# s1, stack #)
{-# NOINLINE pop #-}
{-# SCC pop "prim-pop" #-}

add :: Closure
add stack s0 =
  let !(# s1, v1 #) = popDataStack# stack s0
      !(# s2, v2 #) = popDataStack# stack s1
   in case (# v1, v2 #) of
        (# VInteger# i1, VInteger# i2 #) -> pushDataStack# stack (VInteger# (i2 +# i1)) s2
        _ -> raise# $ toException $ TypeError $ "Expected two ints for reducer '+' (found " <> showValue# v1 <> ", " <> showValue# v2 <> ")"
{-# NOINLINE add #-}
{-# SCC add "prim-add" #-}

times :: Closure
times stack s0 =
  let !(# s1, v1 #) = popDataStack# stack s0
      !(# s2, v2 #) = popDataStack# stack s1
   in case (# v1, v2 #) of
        (# VInteger# i1, VInteger# i2 #) -> pushDataStack# stack (VInteger# (i2 *# i1)) s2
        _ -> raise# $ toException $ TypeError $ "Expected two ints for reducer '*' (found " <> showValue# v1 <> ", " <> showValue# v2 <> ")"
{-# NOINLINE times #-}
{-# SCC times "prim-times" #-}

sub :: Closure
sub stack s0 =
  let !(# s1, v1 #) = popDataStack# stack s0
      !(# s2, v2 #) = popDataStack# stack s1
   in case (# v1, v2 #) of
        (# VInteger# i1, VInteger# i2 #) -> pushDataStack# stack (VInteger# (i2 -# i1)) s2
        _ -> raise# $ toException $ TypeError $ "Expected two ints for reducer '-' (found " <> showValue# v1 <> ", " <> showValue# v2 <> ")"
{-# NOINLINE sub #-}
{-# SCC sub "prim-sub" #-}

dup :: Closure
dup stack s0 =
  let !(# s1, v #) = peekDataStack# stack s0
   in pushDataStack# stack v s1
{-# NOINLINE dup #-}
{-# SCC dup "prim-dup" #-}

swap :: Closure
swap stack s0 =
  let !(# s1, v1 #) = popDataStack# stack s0
      !(# s2, v2 #) = popDataStack# stack s1
      !(# s3, stack0 #) = pushDataStack# stack v1 s2
   in pushDataStack# stack0 v2 s3
{-# NOINLINE swap #-}
{-# SCC swap "prim-swap" #-}

rot31 :: Closure
rot31 stack s0 =
  let !(# s1, v1 #) = popDataStack# stack s0
      !(# s2, v2 #) = popDataStack# stack s1
      !(# s3, v3 #) = popDataStack# stack s2
      !(# s4, stack0 #) = pushDataStack# stack v1 s3
      !(# s5, stack1 #) = pushDataStack# stack0 v3 s4
   in pushDataStack# stack1 v2 s5
{-# NOINLINE rot31 #-}
{-# SCC rot31 "prim-rot31" #-}

rot3_1 :: Closure
rot3_1 stack s0 =
  let !(# s1, v1 #) = popDataStack# stack s0
      !(# s2, v2 #) = popDataStack# stack s1
      !(# s3, v3 #) = popDataStack# stack s2
      !(# s4, stack0 #) = pushDataStack# stack v2 s3
      !(# s5, stack1 #) = pushDataStack# stack0 v3 s4
   in pushDataStack# stack1 v1 s5
{-# NOINLINE rot3_1 #-}
{-# SCC rot3_1 "prim-rot3_1" #-}

ifthenelse :: Closure
ifthenelse stack s0 =
  let !(# s1, vElse #) = popDataStack# stack s0
      !(# s2, vThen #) = popDataStack# stack s1
      !(# s3, vCond #) = popDataStack# stack s2
   in case vCond of
        VBoolean# True# -> pushDataStack# stack vThen s3
        VBoolean# False# -> pushDataStack# stack vElse s3
        _ -> raise# $ toException $ TypeError "Expected boolean as condition for if-then-else"
{-# NOINLINE ifthenelse #-}
{-# SCC ifthenelse "prim-if" #-}

eq :: Closure
eq stack s0 =
  let !(# s1, v1 #) = popDataStack# stack s0
      !(# s2, v2 #) = popDataStack# stack s1
   in case (# v1, v2 #) of
        (# VInteger# i1, VInteger# i2 #) -> pushDataStack# stack (VBoolean# (Bool# (i2 ==# i1))) s2
        (# VDouble# d1, VDouble# d2 #) -> pushDataStack# stack (VBoolean# (Bool# (d2 ==## d1))) s2
        _ -> undefined -- TODO
{-# NOINLINE eq #-}
{-# SCC eq "prim-eq" #-}

lessthan :: Closure
lessthan stack s0 =
  let !(# s1, v1 #) = popDataStack# stack s0
      !(# s2, v2 #) = popDataStack# stack s1
   in case (# v1, v2 #) of
        (# VInteger# i1, VInteger# i2 #) -> pushDataStack# stack (VBoolean# (Bool# (i2 <# i1))) s2
        (# VDouble# d1, VDouble# d2 #) -> pushDataStack# stack (VBoolean# (Bool# (d2 <## d1))) s2
        _ -> undefined -- TODO
{-# NOINLINE lessthan #-}
{-# SCC lessthan "prim-lessthan" #-}
