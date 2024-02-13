{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module        : Utxorpc.Client
-- Description   : Create a connected UTxO RPC client.
-- Create a UTxO RPC client connected to a UTxO RPC service. The @'UtxorpcClient'@ provides functions for each of the methods in the UTxO RPC specification.
-- Provide a UtxorpcClientLogger to perform automated logging.
module Utxorpc.Client
  ( UtxorpcInfo (..),
    utxorpcClient,
    simpleUtxorpcClient,
    utxorpcClientWith,
    UtxorpcClientLogger (..),
    RequestLogger,
    ReplyLogger,
    ServerStreamLogger,
    ServerStreamEndLogger,
  )
where

import qualified Data.ByteString.Char8 as BS
import Network.GRPC.Client (gzip, uncompressed)
import Network.GRPC.Client.Helpers
  ( GrpcClient (_grpcClientHeaders),
    GrpcClientConfig,
    UseTlsOrNot,
    close,
    grpcClientConfigSimple,
    setupGrpcClient,
    _grpcClientConfigCompression,
  )
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Proto.Utxorpc.V1.Build.Build
import Proto.Utxorpc.V1.Submit.Submit
import Proto.Utxorpc.V1.Sync.Sync
import Proto.Utxorpc.V1.Watch.Watch
import Utxorpc.Logged (ReplyLogger, RequestLogger, ServerStreamEndLogger, ServerStreamLogger, UtxorpcClientLogger (..), loggedSStream, loggedUnary)
import Utxorpc.Types
import "http2-client" Network.HTTP2.Client (ClientError, HostName, PortNumber, runClientIO)

-- | Configuration info for a gRPC Service.
-- For more fine-grained control, use @'GrpcClientConfig'@ and @'UtxorpcClientWith'@
data UtxorpcInfo m = UtxorpcInfo
  { _hostName :: HostName,
    _portNumber :: PortNumber,
    _tlsEnabled :: UseTlsOrNot,
    _useGzip :: Bool,
    _clientHeaders :: [(BS.ByteString, BS.ByteString)],
    _logger :: Maybe (UtxorpcClientLogger m)
  }

-- | Make a connection to a UTxO RPC service with the minimum required information.
-- | No logging is performed with the generated service.
simpleUtxorpcClient ::
  HostName ->
  PortNumber ->
  UseTlsOrNot ->
  IO (Either ClientError UtxorpcClient)
simpleUtxorpcClient host port tlsEnabled =
  utxorpcClient $
    UtxorpcInfo host port tlsEnabled False [] Nothing

-- | Make a connection to a UTxO RPC service.
utxorpcClient :: UtxorpcInfo m -> IO (Either ClientError UtxorpcClient)
utxorpcClient
  UtxorpcInfo {_hostName, _portNumber, _tlsEnabled, _useGzip, _logger, _clientHeaders} = do
    eClient <- grpcClient _hostName _portNumber _tlsEnabled _useGzip
    return $ fromGrpc _logger . withHeaders _clientHeaders <$> eClient
    where
      withHeaders hdrs client =
        let oldHdrs = _grpcClientHeaders client
         in client {_grpcClientHeaders = oldHdrs ++ hdrs}

-- | Make a connection to a UTxO RPC using the provided configuration.
utxorpcClientWith ::
  GrpcClientConfig ->
  Maybe (UtxorpcClientLogger m) ->
  IO (Either ClientError UtxorpcClient)
utxorpcClientWith config logger = do
  eClient <- runClientIO $ setupGrpcClient config
  return $ fromGrpc logger <$> eClient

grpcClient ::
  HostName ->
  PortNumber ->
  UseTlsOrNot ->
  Bool ->
  IO (Either ClientError GrpcClient)
grpcClient host port tlsEnabled doCompress = runClientIO $ do
  setupGrpcClient
    ( (grpcClientConfigSimple host port tlsEnabled)
        { _grpcClientConfigCompression = compression
        }
    )
  where
    compression = if doCompress then gzip else uncompressed

fromGrpc :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> UtxorpcClient
fromGrpc logger client =
  UtxorpcClient
    (buildClientImpl logger client)
    (submitClientImpl logger client)
    (syncClientImpl logger client)
    (watchClientImpl logger client)
    (runClientIO $ Network.GRPC.Client.Helpers.close client)

{--------------------------------------
  BUILD
--------------------------------------}

buildClientImpl :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> BuildClientImpl
buildClientImpl logger client =
  BuildClientImpl
    (loggedUnary logger getChainTipRPC client)
    (loggedUnary logger getChainParamRPC client)
    (loggedUnary logger getUtxoByAddressRPC client)
    (loggedUnary logger getUtxoByRefRPC client)
    (loggedSStream logger holdUtxoRPC client)

getChainTipRPC :: RPC LedgerStateService "getChainTip"
getChainTipRPC = RPC

getChainParamRPC :: RPC LedgerStateService "getChainParam"
getChainParamRPC = RPC

getUtxoByAddressRPC :: RPC LedgerStateService "getUtxoByAddress"
getUtxoByAddressRPC = RPC

getUtxoByRefRPC :: RPC LedgerStateService "getUtxoByRef"
getUtxoByRefRPC = RPC

holdUtxoRPC :: RPC LedgerStateService "holdUtxo"
holdUtxoRPC = RPC

{--------------------------------------
  SUBMIT
--------------------------------------}

submitClientImpl :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> SubmitClientImpl
submitClientImpl logger client =
  SubmitClientImpl
    (loggedUnary logger submitTxRPC client)
    (loggedUnary logger readMempoolRPC client)
    (loggedSStream logger waitForTxRPC client)
    (loggedSStream logger watchMempoolRPC client)

submitTxRPC :: RPC SubmitService "submitTx"
submitTxRPC = RPC

readMempoolRPC :: RPC SubmitService "readMempool"
readMempoolRPC = RPC

waitForTxRPC :: RPC SubmitService "waitForTx"
waitForTxRPC = RPC

watchMempoolRPC :: RPC SubmitService "watchMempool"
watchMempoolRPC = RPC

{--------------------------------------
  SYNC
--------------------------------------}

syncClientImpl :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> SyncClientImpl
syncClientImpl logger client =
  SyncClientImpl
    (loggedUnary logger fetchBlockRPC client)
    (loggedUnary logger dumpHistoryRPC client)
    (loggedSStream logger followTipRPC client)

fetchBlockRPC :: RPC ChainSyncService "fetchBlock"
fetchBlockRPC = RPC

dumpHistoryRPC :: RPC ChainSyncService "dumpHistory"
dumpHistoryRPC = RPC

followTipRPC :: RPC ChainSyncService "followTip"
followTipRPC = RPC

{--------------------------------------
  WATCH
--------------------------------------}

watchClientImpl :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> WatchClientImpl
watchClientImpl logger client =
  WatchClientImpl $ loggedSStream logger watchTxRPC client

watchTxRPC :: RPC WatchService "watchTx"
watchTxRPC = RPC
