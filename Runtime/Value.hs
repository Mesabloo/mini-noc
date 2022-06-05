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
    Value# (VQuote#, VDouble#, VInteger#, VCharacter#, VBoolean#),
    showValue#,
    eqValue#,
    decodeValue0#,
    decodeValue1#,
    encodeValue0#,
    Closure,
  )
where

import Runtime.Internal
