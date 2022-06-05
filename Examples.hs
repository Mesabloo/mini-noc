{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Examples where

import Data.Semigroup ((<>))
import Expr (Atom (..), Expr)

example1 :: Expr
example1 = [AIdentifier "pop"]

example2 :: Expr
example2 = [AInteger 5, AInteger 6, AIdentifier "+"]

example3 :: Expr
example3 = example2 <> example2 <> [AIdentifier "+"]

-- | Computation time:
--
--   [Original interpreter] ~6-7us
--   [Optimized VM] ~2.5us
example4 :: Expr
example4 = example3 <> example3 <> [AIdentifier "pop", AIdentifier "dup", AIdentifier "+"]

-- | Computation time:
--
--   [Original interpreter] ~150ms
--   [Optimized VM] ~31ms
example5 :: Expr
example5 = [AInteger 3, AInteger 6, AIdentifier "ack"]

-- | Computation time:
--
--   [Original interpreter] ~26us
--   [Optimized VM] ~5µs
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
--   [Optimized VM] ~107ms
example12 :: Expr
example12 = [AInteger 28, AIdentifier "fib"]
