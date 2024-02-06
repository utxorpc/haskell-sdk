{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Types
  ( UtxorpcService (..),
    BuildServiceImpl (..),
    SubmitServiceImpl (..),
    SyncServiceImpl (..),
    WatchServiceImpl (..),
    ServerStreamCall,
    ServerStreamReply,
    UnaryReply,
  )
where

import Network.GRPC.Client (HeaderList, RawReply)
import Proto.Utxorpc.V1.Build.Build
import Proto.Utxorpc.V1.Submit.Submit
import Proto.Utxorpc.V1.Sync.Sync
import Proto.Utxorpc.V1.Watch.Watch
import "http2-client" Network.HTTP2.Client (ClientError, TooMuchConcurrency)

-- | Type definition for functions that make calls to server stream methods.
-- Note that the stream state, a, can be different for each call.
type ServerStreamCall i o =
  forall a.
  -- | The initial state for the stream processing function.
  a ->
  -- | The request message to send to the service.
  i ->
  -- | The stream processing function. It is a fold over some state a with stream messages o.
  (a -> HeaderList -> o -> IO a) ->
  -- | The final state of the stream processing function, or an error.
  ServerStreamReply a

-- | The type returned by calls to unary service methods.
type UnaryReply o =
  IO
    (Either ClientError (Either TooMuchConcurrency (RawReply o)))

-- | The type returned by calls to server stream methods. a is the final state of the stream processing function.
type ServerStreamReply a =
  IO
    (Either ClientError (Either TooMuchConcurrency (a, HeaderList, HeaderList)))

{---------------------------------------
  UtxorpcService
---------------------------------------}

-- | Service methods for each module in UTxO RPC.
data UtxorpcService = UtxorpcService
  { -- | Build module service methods.
    buildS :: BuildServiceImpl,
    -- | Submit module service methods.
    submitS :: SubmitServiceImpl,
    -- | Sync module service methods.
    syncS :: SyncServiceImpl,
    -- | Watch module service methods.
    watchS :: WatchServiceImpl,
    -- | Closes the gRPC connection.
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
    holdUtxo :: ServerStreamCall HoldUtxoRequest HoldUtxoResponse
  }

{---------------------------------------
  Submit
---------------------------------------}

data SubmitServiceImpl = SubmitServiceImpl
  { submitTx :: SubmitTxRequest -> UnaryReply SubmitTxResponse,
    readMempool :: ReadMempoolRequest -> UnaryReply ReadMempoolResponse,
    waitForTx :: ServerStreamCall WaitForTxRequest WaitForTxResponse,
    watchMempool :: ServerStreamCall WatchMempoolRequest WatchMempoolResponse
  }

{---------------------------------------
  Sync
---------------------------------------}

data SyncServiceImpl = SyncServiceImpl
  { fetchBlock :: FetchBlockRequest -> UnaryReply FetchBlockResponse,
    dumpHistory :: DumpHistoryRequest -> UnaryReply DumpHistoryResponse,
    followTip :: ServerStreamCall FollowTipRequest FollowTipResponse
  }

{---------------------------------------
  Watch
---------------------------------------}

newtype WatchServiceImpl = WatchServiceImpl
  { watchTx :: ServerStreamCall WatchTxRequest WatchTxResponse
  }
