{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module        : Utxorpc.Server
-- Description   : Run a UTxO RPC service.
-- Run UTxO RPC service from a set of method handlers.
-- Provide a @'UtxorpcServiceLogger'@ to perform automated logging.
module Utxorpc.Server
  ( -- * How to use this library
    -- $use

    -- ** Server Stream Methods
    -- $streaming

    -- ** Logging
    -- $logging

    -- * Running a service
    runUtxorpc,
    ServiceConfig (..),
    UtxorpcHandlers (..),
    QueryHandlers (..),
    SubmitHandlers (..),
    SyncHandlers (..),
    WatchHandlers (..),

    -- * Logging
    UtxorpcServiceLogger (..),
    RequestLogger,
    ReplyLogger,
    ServerStreamLogger,
    ServerStreamEndLogger,
  )
where

import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.HTTP2.Encoding (Compression)
import Network.GRPC.Server
import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.WarpTLS (TLSSettings)
import Utxorpc.Query as Query (QueryHandlers (..), serviceHandlers)
import Utxorpc.Logged (ReplyLogger, RequestLogger, ServerStreamEndLogger, ServerStreamLogger, UtxorpcServiceLogger (..))
import Utxorpc.Submit as Submit (SubmitHandlers (..), serviceHandlers)
import Utxorpc.Sync as Sync (SyncHandlers (..), serviceHandlers)
import Utxorpc.Watch as Watch (WatchHandlers (..), serviceHandlers)

-- | Run a UTxO RPC service from a @'ServiceConfig'@.
runUtxorpc ::
  (MonadIO m) =>
  -- | Configuration info and method handlers.
  ServiceConfig m a b c d e ->
  IO ()
runUtxorpc
  ServiceConfig
    { tlsSettings,
      warpSettings,
      handlers,
      logger,
      unlift,
      compression
    } =
    runGrpc
      tlsSettings
      warpSettings
      (Utxorpc.Server.serviceHandlers logger unlift handlers)
      compression

-- | Configuration info and method handlers.
-- Note that the handlers and logger run in the same monad.
-- The monadic actions of the logger and handlers for a single call are combined,
-- and @'unlift'@ runs the combined action in IO. This means that changes to the
-- monadic state made by the request logger (e.g., adding a namespace) are seen by
-- the handlers and other logging functions for that specific call.
data ServiceConfig m a b c d e = ServiceConfig
  { -- | warp-tls settings for using TLS.
    tlsSettings :: TLSSettings,
    -- | warp settings
    warpSettings :: Settings,
    -- | A handler for each method in the UTxO RPC specification.
    handlers :: UtxorpcHandlers m a b c d e,
    -- | Log each RPC event.
    logger :: Maybe (UtxorpcServiceLogger m),
    -- | An unlift function for the handlers and logger. Allows the handler and logger to be run in any monad, but they must be the same monad.
    unlift :: forall x. m x -> IO x,
    -- | A list of compressions to accept and use.
    compression :: [Compression]
  }

-- | A handler for each method in the UTxO RPC specification.
-- @'ServerStreamHandler'@s require a type variable representing the "stream state" (a value that the stream processes/folds over).
-- The type variables here (other than @`m`@) are the type variables of each stream handler in the record.
data
  UtxorpcHandlers
    m -- Monad of the handler functions
    a -- Stream state of `holdUtxo`
    b -- Stream state of `waitForTx`
    c -- Stream state of `watchMempool`
    d -- Stream state of `followTip`
    e -- Stream state of `watchTx`
  = UtxorpcHandlers
  { -- | Handlers for the Query module.
    queryHandlers :: Maybe (QueryHandlers m a),
    -- | Handlers for the Submit module.
    submitHandlers :: Maybe (SubmitHandlers m b c),
    -- | Handlers for the Sync module.
    syncHandlers :: Maybe (SyncHandlers m d),
    -- | Handlers for the Watch module.
    watchHandlers :: Maybe (WatchHandlers m e)
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServiceLogger m) ->
  (forall x. m x -> IO x) ->
  UtxorpcHandlers m a b c d e ->
  [ServiceHandler]
serviceHandlers
  logger
  unlift
  UtxorpcHandlers {queryHandlers, submitHandlers, syncHandlers, watchHandlers} =
    concat . catMaybes $
      [ Query.serviceHandlers logger unlift <$> queryHandlers
      , Submit.serviceHandlers logger unlift <$> submitHandlers
      , Sync.serviceHandlers logger unlift <$> syncHandlers
      , Watch.serviceHandlers logger unlift <$> watchHandlers
      ]

-- $use
-- To run a UTxO RPC service:
--
--     1. Create a `UtxorpcHandlers` record, containing handler for each method in one or more modules in the specification.
--
--     2. Create a `ServiceConfig` record, containing server settings (e.g., TLS settings), the handlers, and (optionally), a logger.
--
--     3. Call `runUtxorpc` with the `ServiceConfig`.

-- $streaming
-- To implement a server stream method, provide a @'ServerStreamHandler'@.
-- Given request metadata and a record of the relevant Message instance,
-- a @'ServerStreamHanlder'@ produces an initial stream state and a streaming function,
-- which folds over the stream state.
-- The stream is closed when the streaming function produces a @'Nothing'@.

-- $logging
-- Automated logging is supported through the @'UtxorpcServiceLogger'@ type.
-- It is a record of one user-defined logging function for each of the following events:
--
-- 1. Request received.
-- 1. Unary reply sent.
--
-- 1. Server stream data sent.
--
-- 1. Server stream ended.
--
-- For more information, see @'ServiceConfig'@, @'UtxorpcServiceLogger'@,
-- and the [`example`](https://github.com/utxorpc/haskell-sdk/tree/main/server/example).

-- $example
-- [`/example`](https://github.com/utxorpc/haskell-sdk/tree/main/server/example) shows how to use the SDK by creating a u5c service with simple handlers that
-- execute a log function and return default (i.e., empty) replies. It demonstrates how to use the SDK
-- without dealing with implementation details of the handlers. It uses one of the following two loggers:
--
--     1. `/example/SimpleLogger.hs` is a simple logger implementation that prints human-readable output.
--
--     1. `/example/KatipLogger.hs` is a more involved logger that demonstrates how to use logging
--     functions that run in a transformer stack. Run the example with `--katip` to use this logger.
--
--         > stack run server-example -- --katip -p=443
