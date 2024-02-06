{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Watch where

import Control.Monad.IO.Class (MonadIO)
import Logging (UtxorpcServerLogger, loggedSStream)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server (ServerStreamHandler, ServiceHandler)
import Proto.Utxorpc.V1.Watch.Watch

newtype WatchHandlers m a = WatchHandlers
  { watchTx :: ServerStreamHandler m WatchTxRequest WatchTxResponse a
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServerLogger m a) ->
  (forall x. m x -> IO x) ->
  WatchHandlers m b ->
  [ServiceHandler]
serviceHandlers logger f WatchHandlers {watchTx} =
  [watchTxSH]
  where
    watchTxSH = loggedSStream logger f (RPC :: RPC WatchService "watchTx") watchTx
