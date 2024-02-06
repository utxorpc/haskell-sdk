{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Client
  ( ServiceInfo (..),
    defaultServiceInfo,
    utxorpcService,
    simpleUtxorpcService,
    utxorpcServiceWith,
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
import Utxorpc.Logged (UtxorpcClientLogger, loggedSStream, loggedUnary)
import Utxorpc.Types
import "http2-client" Network.HTTP2.Client (ClientError, HostName, PortNumber, runClientIO)

data ServiceInfo m = ServiceInfo
  { _hostName :: HostName,
    _portNumber :: PortNumber,
    _tlsEnabled :: UseTlsOrNot,
    _useGzip :: Bool,
    _clientHeaders :: [(BS.ByteString, BS.ByteString)],
    _logger :: Maybe (UtxorpcClientLogger m)
  }

defaultServiceInfo :: HostName -> PortNumber -> UseTlsOrNot -> ServiceInfo m
defaultServiceInfo _hostName _portNumber _tlsEnabled =
  ServiceInfo
    { _hostName,
      _portNumber,
      _tlsEnabled,
      _useGzip = False,
      _clientHeaders = [],
      _logger = Nothing
    }

simpleUtxorpcService :: HostName -> PortNumber -> UseTlsOrNot -> IO (Either ClientError UtxorpcService)
simpleUtxorpcService host port tlsEnabled = utxorpcService $ defaultServiceInfo host port tlsEnabled

utxorpcService :: ServiceInfo m -> IO (Either ClientError UtxorpcService)
utxorpcService
  ServiceInfo {_hostName, _portNumber, _tlsEnabled, _useGzip, _logger, _clientHeaders} = do
    eClient <- mkClient _hostName _portNumber _tlsEnabled _useGzip
    return $ fromClient _logger . withHeaders _clientHeaders <$> eClient
    where
      withHeaders hdrs client =
        let oldHdrs = _grpcClientHeaders client
         in client {_grpcClientHeaders = oldHdrs ++ hdrs}

utxorpcServiceWith ::
  GrpcClientConfig ->
  Maybe (UtxorpcClientLogger m) ->
  IO (Either ClientError UtxorpcService)
utxorpcServiceWith config logger = do
  eClient <- runClientIO $ setupGrpcClient config
  return $ fromClient logger <$> eClient

mkClient ::
  HostName ->
  PortNumber ->
  UseTlsOrNot ->
  Bool ->
  IO (Either ClientError GrpcClient)
mkClient host port tlsEnabled doCompress = runClientIO $ do
  setupGrpcClient
    ( (grpcClientConfigSimple host port tlsEnabled)
        { _grpcClientConfigCompression = compression
        }
    )
  where
    compression = if doCompress then gzip else uncompressed

fromClient :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> UtxorpcService
fromClient logger client =
  UtxorpcService
    (buildServiceImpl logger client)
    (submitServiceImpl logger client)
    (syncServiceImpl logger client)
    (watchServiceImpl logger client)
    (runClientIO $ Network.GRPC.Client.Helpers.close client)

{--------------------------------------
  BUILD
--------------------------------------}

buildServiceImpl :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> BuildServiceImpl
buildServiceImpl logger client =
  BuildServiceImpl
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

submitServiceImpl :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> SubmitServiceImpl
submitServiceImpl logger client =
  SubmitServiceImpl
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

syncServiceImpl :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> SyncServiceImpl
syncServiceImpl logger client =
  SyncServiceImpl
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

watchServiceImpl :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> WatchServiceImpl
watchServiceImpl logger client =
  WatchServiceImpl $ loggedSStream logger watchTxRPC client

watchTxRPC :: RPC WatchService "watchTx"
watchTxRPC = RPC
