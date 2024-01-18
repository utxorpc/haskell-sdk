{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Client (utxorpcService, fromConfig, withHeaders) where

import qualified Data.ByteString as BS
import Logging (UtxorpcClientLogger, loggedSStream, loggedUnary)
import Network.GRPC.Client (gzip, uncompressed)
import Network.GRPC.Client.Helpers (GrpcClient (_grpcClientHeaders), GrpcClientConfig, UseTlsOrNot, grpcClientConfigSimple, setupGrpcClient, _grpcClientConfigCompression)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.HTTP2.Client (ClientIO, HostName, PortNumber)
import Proto.Utxorpc.Build.V1.Build
import Proto.Utxorpc.Submit.V1.Submit
import Proto.Utxorpc.Sync.V1.Sync
import Proto.Utxorpc.Watch.V1.Watch
import Types

utxorpcService ::
  HostName ->
  PortNumber ->
  UseTlsOrNot ->
  Bool ->
  [(BS.ByteString, BS.ByteString)] ->
  Maybe (UtxorpcClientLogger m a) ->
  ClientIO UtxorpcService
utxorpcService host port tlsEnabled doCompress hdrs logger = do
  client <- withHeaders hdrs <$> mkClient host port tlsEnabled doCompress
  return $ fromClient client logger

fromClient :: GrpcClient -> Maybe (UtxorpcClientLogger m a) -> UtxorpcService
fromClient client logger =
  UtxorpcService
    (buildServiceImpl logger client)
    (submitServiceImpl logger client)
    (syncServiceImpl logger client)
    (watchServiceImpl logger client)

mkClient :: HostName -> PortNumber -> UseTlsOrNot -> Bool -> ClientIO GrpcClient
mkClient host port tlsEnabled doCompress = do
  setupGrpcClient
    ( (grpcClientConfigSimple host port tlsEnabled)
        { _grpcClientConfigCompression = compression
        }
    )
  where
    compression = if doCompress then gzip else uncompressed

fromConfig :: GrpcClientConfig -> Maybe (UtxorpcClientLogger m a) -> ClientIO UtxorpcService
fromConfig config logger = do
  client <- setupGrpcClient config
  return $ fromClient client logger

withHeaders :: [(BS.ByteString, BS.ByteString)] -> GrpcClient -> GrpcClient
withHeaders hdrs client =
  let oldHdrs = _grpcClientHeaders client
   in client {_grpcClientHeaders = oldHdrs ++ hdrs}

{--------------------------------------
  BUILD
--------------------------------------}

buildServiceImpl :: Maybe (UtxorpcClientLogger m a) -> GrpcClient -> BuildServiceImpl
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

submitServiceImpl :: Maybe (UtxorpcClientLogger m a) -> GrpcClient -> SubmitServiceImpl
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

syncServiceImpl :: Maybe (UtxorpcClientLogger m a) -> GrpcClient -> SyncServiceImpl
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

watchServiceImpl :: Maybe (UtxorpcClientLogger m a) -> GrpcClient -> WatchServiceImpl
watchServiceImpl logger client =
  WatchServiceImpl $ loggedSStream logger watchTxRPC client

watchTxRPC :: RPC WatchService "watchTx"
watchTxRPC = RPC
