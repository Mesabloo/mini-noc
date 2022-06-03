{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Runtime.Stack
  ( -- * Call stack
    CallStack#,
    newCallStack#,
    pushCallStack#,
    popCallStack#,

    -- * Data stack
    DataStack#,
    newDataStack#,
    pushDataStack#,
    popDataStack#,
    peekDataStack#,
    freezeDataStack#,
  )
where

import Runtime.Internal
