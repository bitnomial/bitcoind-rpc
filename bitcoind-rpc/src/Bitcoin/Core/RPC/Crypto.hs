module Bitcoin.Core.RPC.Crypto (
    globalContext,
) where

import Haskoin.Crypto (Ctx, createContext)
import System.IO.Unsafe (unsafePerformIO)

{- | The global context is created once and never modified again, it is to be passed into cryptographic
functions and contains a number of large data structures that are generated at runtime. Impure functions like
`destroyContext` or `randomizeContext` must not be used against this global value
-}
{-# NOINLINE globalContext #-}
globalContext :: Ctx
globalContext = unsafePerformIO createContext
