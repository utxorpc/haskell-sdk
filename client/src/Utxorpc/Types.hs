{-# LANGUAGE RankNTypes #-}

module Utxorpc.Types
  ( UtxorpcService (..),
    BuildServiceImpl (..),
    SubmitServiceImpl (..),
    SyncServiceImpl (..),
    WatchServiceImpl (..),
    ServerStreamReply,
    UnaryReply,
  )
where

import Network.GRPC.Client (HeaderList, RawReply)
import Network.HTTP2.Client (ClientError, TooMuchConcurrency)
import Proto.Utxorpc.Build.V1.Build
import Proto.Utxorpc.Submit.V1.Submit
import Proto.Utxorpc.Sync.V1.Sync
import Proto.Utxorpc.Watch.V1.Watch

type UnaryReply o =
  IO
    (Either ClientError (Either TooMuchConcurrency (RawReply o)))

type ServerStreamReply a =
  IO
    (Either ClientError (Either TooMuchConcurrency (a, HeaderList, HeaderList)))

{---------------------------------------
  UtxorpcService
---------------------------------------}

data UtxorpcService = UtxorpcService
  { buildS :: BuildServiceImpl,
    submitS :: SubmitServiceImpl,
    syncS :: SyncServiceImpl,
    watchS :: WatchServiceImpl,
    close :: IO (Either ClientError ())
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
      (a -> HeaderList -> HoldUtxoResponse -> IO a) ->
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
      (a -> HeaderList -> WaitForTxResponse -> IO a) ->
      ServerStreamReply a,
    watchMempool ::
      forall a.
      a ->
      WatchMempoolRequest ->
      (a -> HeaderList -> WatchMempoolResponse -> IO a) ->
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
      (a -> HeaderList -> FollowTipResponse -> IO a) ->
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
      (a -> HeaderList -> WatchTxResponse -> IO a) ->
      ServerStreamReply a
  }
