{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Expr (Expr, Atom (..)) where

import Data.Bool (Bool)
import Data.Char (Char)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List qualified as List
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Base (Type)
import GHC.Float (Double)
import GHC.Show (Show (show))

-- | An expression is a list of atoms.
type Expr :: Type
type Expr = [Atom]

instance {-# OVERLAPPING #-} Show Expr where
  show ex = List.unwords $ show <$> ex
  {-# INLINE show #-}

-- | Atoms are uncuttable parts of a program.
type Atom :: Type
data Atom
  = -- | An unevaluated chunk of code, sometimes refered to as a “thunk” or “quote”.
    AQuote Expr
  | -- | A shortcut to an expression already evaluated.
    AIdentifier Text
  | -- | A classic integer literal (most likely to be either 32 or 64-bit).
    AInteger Int
  | -- | A double-precision floating point literal.
    AFloat Double
  | -- | A unicode character.
    ACharacter Char
  | -- | A classic boolean (true or false).
    ABoolean Bool

instance Show Atom where
  show (AQuote ex) = "[" <> show ex <> "]"
  show (AIdentifier id) = Text.unpack id
  show (AInteger i) = show i
  show (AFloat d) = show d
  show (ACharacter c) = show c
  show (ABoolean b) = show b
  {-# INLINE show #-}
