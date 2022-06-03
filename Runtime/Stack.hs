{-# LANGUAGE CPP #-}
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
#if DEBUG == 1
    debugCallStack#,
#endif

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
