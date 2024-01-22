{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Server (runUtxorpc, UtxorpcHandlers (..)) where

import Build (BuildHandlers, serviceHandlers)
import Control.Monad.IO.Class (MonadIO)
import Logging (UtxorpcServerLogger)
import Network.GRPC.HTTP2.Encoding (Compression, gzip)
import Network.GRPC.Server
import Network.Wai.Handler.Warp (Settings)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WarpTLS (TLSSettings, defaultTlsSettings)
import Submit (SubmitHandlers, serviceHandlers)
import Sync (SyncHandlers, serviceHandlers)
import Watch (WatchHandlers, serviceHandlers)

runUtxorpc ::
  (MonadIO m) =>
  TLSSettings ->
  Settings ->
  UtxorpcHandlers m a b c d e ->
  Maybe (UtxorpcServerLogger m f) ->
  [Compression] ->
  IO ()
runUtxorpc tlsSettings settings handlers maybeLogger =
  runGrpc tlsSettings settings (Server.serviceHandlers maybeLogger handlers)

data
  UtxorpcHandlers
    m -- Monad of the handler functions
    a -- Stream state of `holdUtxo`
    b -- Stream state of `waitForTx`
    c -- Stream state of `watchMempool`
    d -- Stream state of `followTip`
    e -- Stream state of `watchTx`
  = UtxorpcHandlers
  { buildHandlers :: BuildHandlers m a,
    submitHandlers :: SubmitHandlers m b c,
    syncHandlers :: SyncHandlers m d,
    watchHandlers :: WatchHandlers m e,
    unlift :: forall x. m x -> IO x
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServerLogger m f) ->
  UtxorpcHandlers m a b c d e ->
  [ServiceHandler]
serviceHandlers
  logger
  UtxorpcHandlers {buildHandlers, submitHandlers, syncHandlers, watchHandlers, unlift} =
    Build.serviceHandlers logger unlift buildHandlers
      <> Submit.serviceHandlers logger unlift submitHandlers
      <> Sync.serviceHandlers logger unlift syncHandlers
      <> Watch.serviceHandlers logger unlift watchHandlers
