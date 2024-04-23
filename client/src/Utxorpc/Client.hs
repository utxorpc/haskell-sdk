{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module        : Utxorpc.Client
-- Description   : Create a connected UTxO RPC client.
-- Create a UTxO RPC client connected to a UTxO RPC service.
-- Provide a UtxorpcClientLogger to perform automated logging.
module Utxorpc.Client
  ( -- * How to use this library
    -- $use

    -- ** Building Messages
    -- $messages

    -- ** Server Stream Methods
    -- $streaming

    -- ** Logging
    -- $logging

    -- ** Examples
    -- $examples

    -- * Creating a @'UtxorpcClient'@
    UtxorpcInfo (..),
    simpleUtxorpcClient,
    utxorpcClient,
    utxorpcClientWith,

    -- * The @'UtxorpcClient'@
    UtxorpcClient (..),
    QueryClient (..),
    SubmitClient (..),
    SyncClient (..),
    WatchClient (..),

    -- ** RPC call function types
    ServerStreamCall,
    ServerStreamReply,
    UnaryReply,

    -- * Logging
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
import Proto.Utxorpc.V1alpha.Query.Query
import Proto.Utxorpc.V1alpha.Submit.Submit
import Proto.Utxorpc.V1alpha.Sync.Sync
import Proto.Utxorpc.V1alpha.Watch.Watch
import Utxorpc.Logged (ReplyLogger, RequestLogger, ServerStreamEndLogger, ServerStreamLogger, UtxorpcClientLogger (..), loggedSStream, loggedUnary)
import Utxorpc.Types
import "http2-client" Network.HTTP2.Client (ClientError, HostName, PortNumber, runClientIO)

-- | Configuration info for a UTxO RPC Client.
-- For more fine-grained control, use @'GrpcClientConfig'@ and @'utxorpcClientWith'@
data UtxorpcInfo m = UtxorpcInfo
  { -- | Host name of the service.
    _hostName :: HostName,
    -- | Port number of the service.
    _portNumber :: PortNumber,
    -- | Whether or not to use TLS.
    _tlsEnabled :: UseTlsOrNot,
    -- | Whether or not to use gzip compression.
    _useGzip :: Bool,
    -- | Headers to include in each request (e.g., API key/authorization).
    _clientHeaders :: [(BS.ByteString, BS.ByteString)],
    -- | Log all RPC events.
    _logger :: Maybe (UtxorpcClientLogger m)
  }

-- | Make a connection to a UTxO RPC service with the minimum required information.
-- No compression is used, no headers are added, and no logging is performed.
-- For more configurability, use @'utxorpcClient'@ or @'utxorpcClientWith'@.
simpleUtxorpcClient ::
  -- | Host name of the service.
  HostName ->
  -- | Port number of the service.
  PortNumber ->
  -- | Whether or not to use TLS.
  UseTlsOrNot ->
  IO (Either ClientError UtxorpcClient)
simpleUtxorpcClient host port tlsEnabled =
  utxorpcClient $
    UtxorpcInfo host port tlsEnabled False [] Nothing

-- | Connect to a UTxO RPC service from a @'UtxorpcInfo'@.
-- Provides more configurability than @'simpleUtxorpcClient'@ but less than @'utxorpcClientWith'@.
utxorpcClient :: UtxorpcInfo m -> IO (Either ClientError UtxorpcClient)
utxorpcClient
  UtxorpcInfo {_hostName, _portNumber, _tlsEnabled, _useGzip, _logger, _clientHeaders} = do
    let sanitizedHost = if _hostName == "localhost" then "127.0.0.1" else _hostName
    eClient <- grpcClient sanitizedHost _portNumber _tlsEnabled _useGzip
    return $ fromGrpc _logger . withHeaders _clientHeaders <$> eClient
    where
      withHeaders hdrs client =
        let oldHdrs = _grpcClientHeaders client
         in client {_grpcClientHeaders = oldHdrs ++ hdrs}

-- | Connect to a UTxO RPC from a @'GrpcClientConfig'@.
-- For a simpler interface with less configurability, use @'utxorpcClient'@ or @'simpleUtxorpcClient'@.
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
    (mkQueryClient logger client)
    (mkSubmitClient logger client)
    (mkSyncClient logger client)
    (mkWatchClient logger client)
    (runClientIO $ Network.GRPC.Client.Helpers.close client)

{--------------------------------------
  QUERY
--------------------------------------}

mkQueryClient :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> QueryClient
mkQueryClient logger client =
  QueryClient
    (loggedUnary logger readParamsRPC client)
    (loggedUnary logger readUtxosRPC client)
    (loggedUnary logger searchUtxosRPC client)
    (loggedSStream logger streamUtxosRPC client)

readParamsRPC :: RPC QueryService "readParams"
readParamsRPC = RPC

readUtxosRPC :: RPC QueryService "readUtxos"
readUtxosRPC = RPC

searchUtxosRPC :: RPC QueryService "searchUtxos"
searchUtxosRPC = RPC

streamUtxosRPC :: RPC QueryService "streamUtxos"
streamUtxosRPC = RPC

{--------------------------------------
  SUBMIT
--------------------------------------}

mkSubmitClient :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> SubmitClient
mkSubmitClient logger client =
  SubmitClient
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

mkSyncClient :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> SyncClient
mkSyncClient logger client =
  SyncClient
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

mkWatchClient :: Maybe (UtxorpcClientLogger m) -> GrpcClient -> WatchClient
mkWatchClient logger client =
  WatchClient $ loggedSStream logger watchTxRPC client

watchTxRPC :: RPC WatchService "watchTx"
watchTxRPC = RPC

-- $use
-- Call any method of a UTxO RPC service through the functions contained in a @'UtxorpcClient'@.
-- Obtain a client by calling one of the client-creating functions:
--
--    1. 'simpleUtxorpcClient' ➤ connect to a service using the bare minimum required information.
--
--        1. See @[quick-start](https://github.com/utxorpc/haskell-sdk/tree/main/client/quick-start)@.
--
--    2. @'utxorpcClient'@ ➤ connect to a service using a @'UtxorpcInfo'@ record.
--
--        1. See @[example](https://github.com/utxorpc/haskell-sdk/tree/main/client/example)@.
--
--    3. @'utxorpcClientWith'@ ➤ provide a @'GrpcClientConfig'@ for fine grained configuration.
--
-- Access the functions of a @'UtxorpcClient'@ through record access:
--
-- > fetchBlock (syncClient client)
--
-- Close the connection throught client's close function:
--
-- > close client

-- $messages
-- To call a UTxO RPC method, you will need a record of the relevant @'Message'@ instance.
-- Build a @'Message'@ with @'defMessage'@ and set its fields with lens operators.
--
-- @
-- import Control.Lens.Operators ((&), (.~))
-- import Data.ProtoLens.Message (Message (defMessage))
-- import qualified Data.Text.Encoding as T
-- import Proto.Utxorpc.V1.Sync.Sync (FetchBlockRequest)
-- import Proto.Utxorpc.V1.Sync.Sync_Fields (hash, index)
--
-- fetchBlockRequest :: FetchBlockRequest
-- fetchBlockRequest =
-- defMessage
--     & ref .~
--     [ defMessage
--         & index .~ 116541970
--         & hash .~ T.encodeUtf8 "9d5abce5b1a7d94b141a597fd621a1ff9dcd46579ff1939664364311cd1be338"
--     ]
-- @

-- $streaming
-- Note that calling a server-stream method requires an input-stream function and initial input-stream state.
-- The input-stream function is of type @(a -> 'HeaderList' -> o -> IO a)@, where @a@ is the initial input-stream
-- state and @o@ is the type of message streamed by the server. The input-stream function folds over its state
-- until the stream is closed.

-- $logging
-- This SDK supports automated logging through the @'UtxorpcClientLogger'@ type.
-- It is a record of one user-defined logging function for each of the following events:
--
--     1. Request sent.
--
--     2. Reply received.
--
--     3. Server stream data received.
--
--     4. Server stream ended.
--
-- For more information, see @'UtxorpcClientLogger'@ and the examples.

-- $examples
-- There are two examples included in the [project](https://github.com/utxorpc/haskell-sdk).
-- There are two provided examples:
--
--     1. `/quick-start` shows the bare minimum required to make a single unary request.
--
--         > stack run client-quick-start -- -p 443
--     2. `/example` shows a more involved example that uses one of the following two logger implementations:
--
--         1. `/example/SimpleLogger.hs` is a simple logger implementation that prints human-readable output.
--
--             > stack run client-example
--             > Usage: [--katip] <hostName> <port> <tlsEnabled> <useGzip> [<headerKey>:<headerValue> [...]]
--             > stack run client-example -- "localhost" 443 True False
--
--         2. `/example/KatipLogger.hs` is a more involved logger that demonstrates how to use logging functions
--         that run in a transformer stack. Run the example with `--katip` to use this logger.
--
--             > stack run client-example -- --katip "localhost" 443 True False
