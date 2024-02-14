{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Watch (WatchHandlers (..), serviceHandlers) where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server (ServerStreamHandler, ServiceHandler)
import Proto.Utxorpc.V1alpha.Watch.Watch
import Utxorpc.Logged (UtxorpcServiceLogger, loggedSStream)

newtype WatchHandlers m a = WatchHandlers
  { watchTx :: ServerStreamHandler m WatchTxRequest WatchTxResponse a
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServiceLogger m) ->
  (forall x. m x -> IO x) ->
  WatchHandlers m b ->
  [ServiceHandler]
serviceHandlers logger f WatchHandlers {watchTx} =
  [watchTxSH]
  where
    watchTxSH = loggedSStream f (RPC :: RPC WatchService "watchTx") watchTx logger
