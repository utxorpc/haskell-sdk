{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Server
  ( runUtxorpc,
    UtxorpcHandlers (..),
    BuildHandlers (..),
    SubmitHandlers (..),
    SyncHandlers (..),
    WatchHandlers (..),
    UtxorpcServerLogger (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.HTTP2.Encoding (Compression)
import Network.GRPC.Server
import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.WarpTLS (TLSSettings)
import Utxorpc.Build as Build (BuildHandlers (..), serviceHandlers)
import Utxorpc.Logged (UtxorpcServerLogger (..))
import Utxorpc.Submit as Submit (SubmitHandlers (..), serviceHandlers)
import Utxorpc.Sync as Sync (SyncHandlers (..), serviceHandlers)
import Utxorpc.Watch as Watch (WatchHandlers (..), serviceHandlers)

runUtxorpc ::
  (MonadIO m) =>
  TLSSettings ->
  Settings ->
  UtxorpcHandlers m a b c d e ->
  Maybe (UtxorpcServerLogger m f) ->
  [Compression] ->
  IO ()
runUtxorpc tlsSettings settings handlers maybeLogger =
  runGrpc tlsSettings settings (Utxorpc.Server.serviceHandlers maybeLogger handlers)

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
