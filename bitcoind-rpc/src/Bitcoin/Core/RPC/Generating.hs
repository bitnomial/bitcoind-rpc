{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bitcoin.Core.RPC.Generating (
    generateToAddress,
) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Word (Word32)
import Haskoin.Block (BlockHash)

import Servant.Bitcoind (
    BitcoindClient,
    BitcoindEndpoint,
    C,
    I,
    O,
    toBitcoindClient,
 )

type GeneratingRpc = BitcoindEndpoint "generatetoaddress" (I Word32 -> I Text -> O Word32 -> C [BlockHash])

-- | Generate blocks in @regtest@ mode
generateToAddress ::
    -- | number of blocks to generate
    Word32 ->
    -- | address for the coinbase reward
    Text ->
    -- | how many iterations to try
    Maybe Word32 ->
    BitcoindClient [BlockHash]
generateToAddress = toBitcoindClient $ Proxy @GeneratingRpc
