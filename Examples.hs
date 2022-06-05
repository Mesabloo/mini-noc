{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Examples where

import Data.Semigroup ((<>))
import Expr (Atom (..), Expr)

example1 :: Expr
example1 = [AIdentifier "pop"]
{-# NOINLINE example1 #-}

example2 :: Expr
example2 = [AInteger 5, AInteger 6, AIdentifier "+"]
{-# NOINLINE example2 #-}

example3 :: Expr
example3 = example2 <> example2 <> [AIdentifier "+"]
{-# NOINLINE example3 #-}

-- | Computation time:
--
--   [Original interpreter] ~6-7us
--   [Optimized VM] ~1.8us
example4 :: Expr
example4 = example3 <> example3 <> [AIdentifier "pop", AIdentifier "dup", AIdentifier "+"]
{-# NOINLINE example4 #-}

-- | Computation time:
--
--   [Original interpreter] ~150ms
--   [Optimized VM] ~26ms
example5 :: Expr
example5 = [AInteger 3, AInteger 6, AIdentifier "ack"]
{-# NOINLINE example5 #-}

-- | Computation time:
--
--   [Original interpreter] ~26us
--   [Optimized VM] ~4µs
example6 :: Expr
example6 = [AInteger 15, AIdentifier "fact"]
{-# NOINLINE example6 #-}

example7 :: Expr
example7 = [AInteger 15, AInteger 6, AInteger 25, AInteger 15]
{-# NOINLINE example7 #-}

example8 :: Expr
example8 = [AQuote [AInteger 1], AInteger 2]
{-# NOINLINE example8 #-}

example9 :: Expr
example9 = example8 <> [AIdentifier "pop", AIdentifier "unquote"]
{-# NOINLINE example9 #-}

example10 :: Expr
example10 = [AQuote [AQuote [AInteger 10], AIdentifier "unquote"], AIdentifier "unquote"]
{-# NOINLINE example10 #-}

example11 :: Expr
example11 = [AQuote [AInteger 10, AQuote [AInteger 12], AIdentifier "unquote"], AIdentifier "unquote"]
{-# NOINLINE example11 #-}

-- | Computation time:
--
--   [Original interpreter] ???
--   [Optimized VM] ~92ms
example12 :: Expr
example12 = [AInteger 28, AIdentifier "fib"]
{-# NOINLINE example12 #-}

example13 :: Expr
example13 = [AInteger 3, AInteger 7, AIdentifier "ack"]
{-# NOINLINE example13 #-}
