{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Bitcoind (
    -- * Bitcoind api endpoint DSL
    C,
    CX,
    F,
    I,
    O,

    -- * Types related to defaulting
    EmptyString,
    EmptyList,
    DefFalse,
    DefZero,

    -- * Types related to the client
    BitcoindClient,
    BitcoindEndpoint,
    BitcoindException (..),

    -- * Client generation mechanism
    HasBitcoindClient (..),
    Rewrite (..),

    -- * Utility functions
    utcTime,
    toSatoshis,
) where

import Control.Exception (Exception)
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import qualified Data.Aeson.Types as Ae
import Data.Bifunctor (first)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32, Word64)
import GHC.TypeLits (KnownSymbol, Symbol)
import Servant.API ((:<|>) (..), (:>))
import Servant.API.BasicAuth (BasicAuth, BasicAuthData)
import Servant.Client (ClientError, ClientM, client)
import Servant.Client.JsonRpc (
    JsonRpc,
    JsonRpcErr (..),
    JsonRpcResponse (..),
 )

-- | Exceptions resulting from interacting with bitcoind
data BitcoindException
    = -- | The error message returned by bitcoind on failure
      RpcException String
    | ClientException ClientError
    | DecodingError String
    deriving (Show)

instance Exception BitcoindException

data BitcoindEndpoint (m :: Symbol) a

-- | A client returning @Either BitcoindException r@
data C r

-- | A client returning @Either BitcoindException ()@
data CX

-- | An argument with a fixed value
data F x r

-- | An optional argument
data O r

-- | An ordinary argument
data I r

class HasDefault x a where
    getDefault :: p x -> a

data EmptyString

instance HasDefault EmptyString Text where getDefault _ = ""

data DefFalse

instance HasDefault DefFalse Bool where getDefault _ = False

data EmptyList

instance HasDefault EmptyList [a] where getDefault _ = []

data DefZero

instance Num a => HasDefault DefZero a where getDefault _ = 0

class HasBitcoindClient x where
    type TheBitcoindClient x :: *
    toBitcoindClient :: p x -> TheBitcoindClient x

instance
    (Rewrite a, RewriteFrom a ~ NakedClient, KnownSymbol m) =>
    HasBitcoindClient (BitcoindEndpoint m a)
    where
    type TheBitcoindClient (BitcoindEndpoint m a) = RewriteTo a
    toBitcoindClient _ =
        rewriteRpc (Proxy @a)
            . client
            $ Proxy @(BitcoindRpc m)

instance
    (HasBitcoindClient x, HasBitcoindClient y) =>
    HasBitcoindClient (x :<|> y)
    where
    type TheBitcoindClient (x :<|> y) = TheBitcoindClient x :<|> TheBitcoindClient y
    toBitcoindClient _ = toBitcoindClient (Proxy @x) :<|> toBitcoindClient (Proxy @y)

type BitcoindRpc m = BasicAuth "bitcoind" () :> JsonRpc m [Value] String Value

type BitcoindClient r = ReaderT BasicAuthData (ExceptT BitcoindException ClientM) r

type NakedClient =
    BasicAuthData ->
    [Value] ->
    ClientM (JsonRpcResponse String Value)

{- | Bitcoind uses JSON arrays to serialize parameters.  This typeclass
 describes a generic rewriting system, but we apply it here to transform
 clients of the form @BasicAuthData -> [Value] -> ClientM Value@ into curried
 functions with endpoint specific arguments.
-}
class Rewrite a where
    type RewriteFrom a :: *
    type RewriteTo a :: *
    rewriteRpc :: p a -> RewriteFrom a -> RewriteTo a

-- | Handle endpoints which do not have an expected return value
instance Rewrite CX where
    type RewriteFrom CX = NakedClient
    type RewriteTo CX = BitcoindClient ()

    rewriteRpc _ f = ReaderT $ ExceptT . fmap repack . (`f` [])
      where
        repack = \case
            Ack _ -> return ()
            Errors _ (JsonRpcErr _ e _) -> Left $ RpcException e
            Result{} -> Left $ RpcException "Expecting ack; got result"

-- | Endpoints which simply return a value
instance FromJSON r => Rewrite (C r) where
    type RewriteFrom (C r) = NakedClient
    type RewriteTo (C r) = BitcoindClient r

    rewriteRpc _ f = ReaderT $ ExceptT . fmap repack . (`f` [])
      where
        repack = \case
            Result _ x -> first DecodingError $ Ae.parseEither parseJSON x
            Errors _ (JsonRpcErr _ e _) -> Left $ RpcException e
            Ack{} -> Left $ RpcException "Expecting result; got ack"

-- | Add a normal argument
instance
    (RewriteFrom b ~ NakedClient, Rewrite b, ToJSON a) =>
    Rewrite (I a -> b)
    where
    type RewriteFrom (I a -> b) = NakedClient
    type RewriteTo (I a -> b) = a -> RewriteTo b

    rewriteRpc _ f x = rewriteRpc (Proxy @b) $ \auth args -> f auth (toJSON x : args)

-- | Add an optional argument
instance
    (RewriteFrom b ~ NakedClient, Rewrite b, ToJSON a) =>
    Rewrite (O a -> b)
    where
    type RewriteFrom (O a -> b) = NakedClient
    type RewriteTo (O a -> b) = Maybe a -> RewriteTo b

    rewriteRpc _ f x = rewriteRpc (Proxy @b) $ \auth args -> f auth ((toJSON <$> x) `maybeCons` args)

-- | Add a fixed argument
instance
    (RewriteFrom b ~ NakedClient, Rewrite b, ToJSON a, HasDefault x a) =>
    Rewrite (F x a -> b)
    where
    type RewriteFrom (F x a -> b) = NakedClient
    type RewriteTo (F x a -> b) = RewriteTo b

    rewriteRpc _ f = rewriteRpc (Proxy @b) f'
      where
        f' auth args = f auth $ fixedVal : args
        fixedVal = toJSON @a . getDefault $ Proxy @x

instance (Rewrite a, Rewrite b) => Rewrite (a :<|> b) where
    type RewriteFrom (a :<|> b) = RewriteFrom a :<|> RewriteFrom b
    type RewriteTo (a :<|> b) = RewriteTo a :<|> RewriteTo b
    rewriteRpc _ (x :<|> y) = rewriteRpc (Proxy @a) x :<|> rewriteRpc (Proxy @b) y

maybeCons :: Maybe a -> [a] -> [a]
maybeCons mx xs = maybe xs (: xs) mx

-- | Helper function for decoding POSIX timestamps
utcTime :: Word64 -> UTCTime
utcTime = posixSecondsToUTCTime . fromIntegral

-- | Convert BTC to Satoshis
toSatoshis :: Scientific -> Word32
toSatoshis = floor . (* 100_000_000)
