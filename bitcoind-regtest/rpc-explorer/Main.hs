{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Applicative (optional, (<**>))
import Control.Concurrent (threadDelay)
import Control.Monad ((>=>))
import Control.Monad.Fix (fix)
import Data.ByteString.Char8 (unpack)
import Data.List (isPrefixOf, sort)
import Data.Maybe (listToMaybe, mapMaybe)
import Options.Applicative (
    ParserInfo,
    execParser,
    help,
    helper,
    info,
    long,
    progDesc,
    short,
    strOption,
 )
import Servant.API (BasicAuthData (..))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Bitcoin.Core.Regtest (
    NodeHandle,
    nodeAuth,
    nodePort,
    withBitcoind,
 )

opts :: ParserInfo (Maybe FilePath)
opts = info (outputOpt <**> helper) $ progDesc "Create a representation of the bitcoind rpc command set"
  where
    outputOpt =
        optional . strOption $
            short 'o' <> long "output" <> help "Omitting this argument outputs to stdout"

main :: IO ()
main = do
    outputFile <- execParser opts
    mapM_ (`writeFile` mempty) outputFile
    let writeOutput = maybe putStrLn appendFile outputFile
    withBitcoind 8330 $ \h -> do
        rpcCommands <- parseHelpText <$> bitcoinCli h ["help"]
        writeOutput $ unlines rpcCommands
        mapM_ (getHelpText h >=> writeOutput) rpcCommands
  where
    getHelpText h c = bitcoinCli h ["help", c]

-- | Compute the sorted list of available RPC commands
parseHelpText :: String -> [String]
parseHelpText = sort . mapMaybe (listToMaybe . words) . filter isRpc . lines
  where
    isRpc = not . isPrefixOf "="

bitcoinCli :: NodeHandle -> [String] -> IO String
bitcoinCli h args = fix $ \go -> do
    (code, output, _) <- readProcessWithExitCode "bitcoin-cli" (setupArgs <> args) mempty
    if code == ExitSuccess
        then pure output
        else threadDelay 1_000_000 >> go
  where
    setupArgs =
        [ "-rpcport=" <> show (nodePort h)
        , "-rpcuser=" <> (unpack . basicAuthUsername . nodeAuth) h
        , "-rpcpassword=" <> (unpack . basicAuthPassword . nodeAuth) h
        ]
