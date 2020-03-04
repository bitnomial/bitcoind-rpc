{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Bitcoin.Core.RPC.Generating
    ( generateToAddress
    ) where

import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Data.Word             (Word32)
import           Network.Haskoin.Block (BlockHash)

import           Servant.Bitcoind      (BitcoindClient, BitcoindEndpoint, C, I,
                                        O, toBitcoindClient)


type GeneratingRpc = BitcoindEndpoint "generatetoaddress" (I Word32 -> I Text -> O Word32 -> C [BlockHash])


-- | Generate blocks in @regtest@ mode
generateToAddress
  :: Word32
  -- ^ number of blocks to generate
  -> Text
  -- ^ address for the coinbase reward
  -> Maybe Word32
  -- ^ how many iterations to try
  -> BitcoindClient [BlockHash]
generateToAddress = toBitcoindClient $ Proxy @GeneratingRpc
