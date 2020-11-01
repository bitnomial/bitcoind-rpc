{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Bitcoin.Core.RPC.Control (
    stop,
    uptime,
) where

import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Servant.API ((:<|>) (..))

import Servant.Bitcoind (
    BitcoindClient,
    BitcoindEndpoint,
    C,
    CX,
    toBitcoindClient,
 )

type ControlRpc =
    BitcoindEndpoint "stop" CX
        :<|> BitcoindEndpoint "uptime" (C Word32)

-- | Request a graceful shutdown of Bitcoin Core.
stop :: BitcoindClient ()

-- | Returns the total uptime of the server (in seconds)
uptime :: BitcoindClient Word32
stop :<|> uptime = toBitcoindClient $ Proxy @ControlRpc
