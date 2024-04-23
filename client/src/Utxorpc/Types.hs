{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module       : Utxorpc.Types
-- Description  : Record types and type aliases.
-- The types in this module are required to call methods of a `UtxorpcClient`.
module Utxorpc.Types
  ( UtxorpcClient (..),
    QueryClient (..),
    SubmitClient (..),
    SyncClient (..),
    WatchClient (..),
    ServerStreamCall,
    ServerStreamReply,
    UnaryReply,
  )
where

import Network.GRPC.Client (HeaderList, RawReply)
import Proto.Utxorpc.V1alpha.Query.Query
import Proto.Utxorpc.V1alpha.Submit.Submit
import Proto.Utxorpc.V1alpha.Sync.Sync
import Proto.Utxorpc.V1alpha.Watch.Watch
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
  UtxorpcClient
---------------------------------------}

-- | Methods for each module in UTxO RPC.
--
-- >>> fetchBlock (queryClient client) defMessage
data UtxorpcClient = UtxorpcClient
  { -- | Query module service methods.
    queryClient :: QueryClient,
    -- | Submit module service methods.
    submitClient :: SubmitClient,
    -- | Sync module service methods.
    syncClient :: SyncClient,
    -- | Watch module service methods.
    watchClient :: WatchClient,
    -- | Closes the gRPC connection.
    close :: IO (Either ClientError ())
  }

{---------------------------------------
  Query
---------------------------------------}

-- | Methods of the Query module
data QueryClient = QueryClient
  { readParams :: ReadParamsRequest -> UnaryReply ReadParamsResponse,
    readUtxos :: ReadUtxosRequest -> UnaryReply ReadUtxosResponse,
    searchUtxos :: SearchUtxosRequest -> UnaryReply SearchUtxosResponse,
    streamUtxos :: ServerStreamCall ReadUtxosRequest ReadUtxosResponse
  }

{---------------------------------------
  Submit
---------------------------------------}

-- | Methods of the Submit module
data SubmitClient = SubmitClient
  { submitTx :: SubmitTxRequest -> UnaryReply SubmitTxResponse,
    readMempool :: ReadMempoolRequest -> UnaryReply ReadMempoolResponse,
    waitForTx :: ServerStreamCall WaitForTxRequest WaitForTxResponse,
    watchMempool :: ServerStreamCall WatchMempoolRequest WatchMempoolResponse
  }

{---------------------------------------
  Sync
---------------------------------------}

-- | Methods of the Sync module
data SyncClient = SyncClient
  { fetchBlock :: FetchBlockRequest -> UnaryReply FetchBlockResponse,
    dumpHistory :: DumpHistoryRequest -> UnaryReply DumpHistoryResponse,
    followTip :: ServerStreamCall FollowTipRequest FollowTipResponse
  }

{---------------------------------------
  Watch
---------------------------------------}

-- | Methods of the watch module
newtype WatchClient = WatchClient
  { watchTx :: ServerStreamCall WatchTxRequest WatchTxResponse
  }
