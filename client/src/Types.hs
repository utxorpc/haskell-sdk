{-# LANGUAGE RankNTypes #-}

module Types where

import Network.GRPC.Client (HeaderList, RawReply)
import Network.HTTP2.Client (ClientIO, TooMuchConcurrency)
import Proto.Utxorpc.Build.V1.Build
import Proto.Utxorpc.Submit.V1.Submit
import Proto.Utxorpc.Sync.V1.Sync
import Proto.Utxorpc.Watch.V1.Watch

type UnaryReply o = ClientIO (Either TooMuchConcurrency (RawReply o))

type ServerStreamReply a = ClientIO (Either TooMuchConcurrency (a, HeaderList, HeaderList))

{---------------------------------------
  UtxorpcService
---------------------------------------}

data UtxorpcService = UtxorpcService
  { buildS :: BuildServiceImpl,
    submitS :: SubmitServiceImpl,
    syncS :: SyncServiceImpl,
    watchS :: WatchServiceImpl,
    close :: ClientIO ()
  }

{---------------------------------------
  Build
---------------------------------------}

data BuildServiceImpl = BuildServiceImpl
  { getChainTip :: GetChainTipRequest -> UnaryReply GetChainTipResponse,
    getChainParam :: GetChainParamRequest -> UnaryReply GetChainParamResponse,
    getUtxoByAddress :: GetUtxoByAddressRequest -> UnaryReply GetUtxoByAddressResponse,
    getUtxoByRef :: GetUtxoByRefRequest -> UnaryReply GetUtxoByRefResponse,
    holdUtxo ::
      forall a.
      a ->
      HoldUtxoRequest ->
      (a -> HeaderList -> HoldUtxoResponse -> ClientIO a) ->
      ServerStreamReply a
  }

{---------------------------------------
  Submit
---------------------------------------}

data SubmitServiceImpl = SubmitServiceImpl
  { submitTx :: SubmitTxRequest -> UnaryReply SubmitTxResponse,
    readMempool :: ReadMempoolRequest -> UnaryReply ReadMempoolResponse,
    waitForTx ::
      forall a.
      a ->
      WaitForTxRequest ->
      (a -> HeaderList -> WaitForTxResponse -> ClientIO a) ->
      ServerStreamReply a,
    watchMempool ::
      forall a.
      a ->
      WatchMempoolRequest ->
      (a -> HeaderList -> WatchMempoolResponse -> ClientIO a) ->
      ServerStreamReply a
  }

{---------------------------------------
  Sync
---------------------------------------}

data SyncServiceImpl = SyncServiceImpl
  { fetchBlock :: FetchBlockRequest -> UnaryReply FetchBlockResponse,
    dumpHistory :: DumpHistoryRequest -> UnaryReply DumpHistoryResponse,
    followTip ::
      forall a.
      a ->
      FollowTipRequest ->
      (a -> HeaderList -> FollowTipResponse -> ClientIO a) ->
      ServerStreamReply a
  }

{---------------------------------------
  Watch
---------------------------------------}

newtype WatchServiceImpl = WatchServiceImpl
  { watchTx ::
      forall a.
      a ->
      WatchTxRequest ->
      (a -> HeaderList -> WatchTxResponse -> ClientIO a) ->
      ServerStreamReply a
  }
