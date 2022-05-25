{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    DefTrue,
    DefZero,

    -- * Types related to the client
    BitcoindClient (..),
    WalletName,
    BitcoindEndpoint,
    BitcoindWalletEndpoint,
    BitcoindException (..),

    -- * Client generation mechanism
    HasBitcoindClient (..),
    Rewrite (..),
) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT, get)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (Null),
    object,
    (.=),
 )
import qualified Data.Aeson.Types as Ae
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, Symbol)
import Servant.API (CaptureAll, (:<|>) (..), (:>))
import Servant.API.BasicAuth (BasicAuth, BasicAuthData)
import Servant.Client (Client, ClientError, ClientM, client)
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

instance ToJSON BitcoindException where
    toJSON err = object ["type" .= theType, "error" .= theError]
      where
        (theType, theError) = case err of
            RpcException rpcError -> ("RpcException" :: Text, rpcError)
            ClientException clientError -> ("ClientException", show clientError)
            DecodingError decodingError -> ("DecodingError", decodingError)

instance Exception BitcoindException

data BitcoindEndpoint (m :: Symbol) a
data BitcoindWalletEndpoint (m :: Symbol) a

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

data DefTrue

instance HasDefault DefTrue Bool where getDefault _ = True

data EmptyList

instance HasDefault EmptyList [a] where getDefault _ = []

data DefZero

instance Num a => HasDefault DefZero a where getDefault _ = 0

class HasBitcoindClient x where
    type TheBitcoindClient x :: Type
    toBitcoindClient :: p x -> TheBitcoindClient x

instance
    (Rewrite a, RewriteFrom a ~ NakedClient, KnownSymbol m) =>
    HasBitcoindClient (BitcoindEndpoint m a)
    where
    type TheBitcoindClient (BitcoindEndpoint m a) = RewriteTo a
    toBitcoindClient _ =
        rewriteRpc (Proxy @a)
            . ignorePath
            . trimArguments
            . client
            $ Proxy @(BitcoindRpc m)
      where
        ignorePath f auth _ = f auth mempty

instance
    (Rewrite a, RewriteFrom a ~ NakedClient, KnownSymbol m) =>
    HasBitcoindClient (BitcoindWalletEndpoint m a)
    where
    type TheBitcoindClient (BitcoindWalletEndpoint m a) = RewriteTo a
    toBitcoindClient _ =
        rewriteRpc (Proxy @a)
            . trimArguments
            . client
            $ Proxy @(BitcoindRpc m)

trimArguments :: Client m (BitcoindRpc api) -> Client m (BitcoindRpc api)
trimArguments f authData path args = f authData path $ dropTrailingNothings args

dropTrailingNothings :: [Value] -> [Value]
dropTrailingNothings [] = []
dropTrailingNothings (x : xs) = case dropTrailingNothings xs of
    []
        | x == Null -> []
        | otherwise -> [x]
    adjustedTail -> x : adjustedTail

instance
    (HasBitcoindClient x, HasBitcoindClient y) =>
    HasBitcoindClient (x :<|> y)
    where
    type TheBitcoindClient (x :<|> y) = TheBitcoindClient x :<|> TheBitcoindClient y
    toBitcoindClient _ = toBitcoindClient (Proxy @x) :<|> toBitcoindClient (Proxy @y)

type BitcoindRpc m =
    BasicAuth "bitcoind" ()
        :> CaptureAll "wallet" Text
        :> JsonRpc m [Value] String Value

type WalletName = Text

newtype BitcoindClient a = BitcoindClient
    { unBitcoindClient :: ReaderT BasicAuthData (StateT (Maybe WalletName) (ExceptT BitcoindException ClientM)) a
    }
    deriving (Functor, Applicative, Monad, MonadIO)

getWalletPath :: Monad m => StateT (Maybe WalletName) m [Text]
getWalletPath = maybe mempty toPath <$> get
  where
    toPath name = ["wallet", name]

type NakedClient =
    BasicAuthData ->
    [Text] ->
    [Value] ->
    ClientM (JsonRpcResponse String Value)

{- | Bitcoind uses JSON arrays to serialize parameters.  This typeclass
 describes a generic rewriting system, but we apply it here to transform
 clients of the form @BasicAuthData -> [Value] -> ClientM Value@ into curried
 functions with endpoint specific arguments.
-}
class Rewrite a where
    type RewriteFrom a :: Type
    type RewriteTo a :: Type
    rewriteRpc :: p a -> RewriteFrom a -> RewriteTo a

-- | Handle endpoints which do not have an expected return value
instance Rewrite CX where
    type RewriteFrom CX = NakedClient
    type RewriteTo CX = BitcoindClient ()

    rewriteRpc _ f = BitcoindClient . ReaderT $ \auth -> do
        path <- getWalletPath
        lift . ExceptT $ repack <$> f auth path mempty
      where
        repack = \case
            Ack _ -> return ()
            Errors _ (JsonRpcErr _ e _) -> Left $ RpcException e
            Result{} -> Left $ RpcException "Expecting ack; got result"

-- | Endpoints which simply return a value
instance FromJSON r => Rewrite (C r) where
    type RewriteFrom (C r) = NakedClient
    type RewriteTo (C r) = BitcoindClient r

    rewriteRpc _ f = BitcoindClient . ReaderT $ \auth -> do
        path <- getWalletPath
        lift . ExceptT $ repack <$> f auth path mempty
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

    rewriteRpc _ f x = rewriteRpc (Proxy @b) $ \auth path args ->
        f auth path (toJSON x : args)

-- | Add an optional argument
instance
    (RewriteFrom b ~ NakedClient, Rewrite b, ToJSON a) =>
    Rewrite (O a -> b)
    where
    type RewriteFrom (O a -> b) = NakedClient
    type RewriteTo (O a -> b) = Maybe a -> RewriteTo b

    rewriteRpc _ f x = rewriteRpc (Proxy @b) $ \auth path args ->
        f auth path (toJSON x : args)

-- | Add a fixed argument
instance
    (RewriteFrom b ~ NakedClient, Rewrite b, ToJSON a, HasDefault x a) =>
    Rewrite (F x a -> b)
    where
    type RewriteFrom (F x a -> b) = NakedClient
    type RewriteTo (F x a -> b) = RewriteTo b

    rewriteRpc _ f = rewriteRpc (Proxy @b) f'
      where
        f' auth path args = f auth path $ fixedVal : args
        fixedVal = toJSON @a . getDefault $ Proxy @x

instance (Rewrite a, Rewrite b) => Rewrite (a :<|> b) where
    type RewriteFrom (a :<|> b) = RewriteFrom a :<|> RewriteFrom b
    type RewriteTo (a :<|> b) = RewriteTo a :<|> RewriteTo b
    rewriteRpc _ (x :<|> y) = rewriteRpc (Proxy @a) x :<|> rewriteRpc (Proxy @b) y
