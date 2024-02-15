{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Server
  ( runUtxorpc,
    ServiceConfig (..),
    UtxorpcHandlers (..),
    BuildHandlers (..),
    SubmitHandlers (..),
    SyncHandlers (..),
    WatchHandlers (..),
    UtxorpcServiceLogger (..),
    RequestLogger,
    ReplyLogger,
    ServerStreamLogger,
    ServerStreamEndLogger,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.HTTP2.Encoding (Compression)
import Network.GRPC.Server
import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.WarpTLS (TLSSettings)
import Utxorpc.Build as Build (BuildHandlers (..), serviceHandlers)
import Utxorpc.Logged (ReplyLogger, RequestLogger, ServerStreamEndLogger, ServerStreamLogger, UtxorpcServiceLogger (..))
import Utxorpc.Submit as Submit (SubmitHandlers (..), serviceHandlers)
import Utxorpc.Sync as Sync (SyncHandlers (..), serviceHandlers)
import Utxorpc.Watch as Watch (WatchHandlers (..), serviceHandlers)

-- | Run a UTxO RPC service from a @'ServiceConfig'@.
runUtxorpc ::
  (MonadIO m) =>
  -- | Configuration info and method handlers. See @'ServiceConfig'@ for type information.
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
  { -- | Handlers for the Build module.
    buildHandlers :: BuildHandlers m a,
    -- | Handlers for the Submit module.
    submitHandlers :: SubmitHandlers m b c,
    -- | Handlers for the Sync module.
    syncHandlers :: SyncHandlers m d,
    -- | Handlers for the Watch module.
    watchHandlers :: WatchHandlers m e
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
  UtxorpcHandlers {buildHandlers, submitHandlers, syncHandlers, watchHandlers} =
    Build.serviceHandlers logger unlift buildHandlers
      <> Submit.serviceHandlers logger unlift submitHandlers
      <> Sync.serviceHandlers logger unlift syncHandlers
      <> Watch.serviceHandlers logger unlift watchHandlers
