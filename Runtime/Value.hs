{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Runtime.Value
  ( -- * Unboxed, unlifted booleans
    Bool# (Bool#, True#, False#),
    showBool#,

    -- * Values
    Value# (VQuote#, VDouble#, VInteger#, VCharacter#, VBoolean#, VPrimitive#),
    showValue#,
    eqValue#,
    Closure,
  )
where

import Runtime.Internal
