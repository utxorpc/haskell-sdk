{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Sync (SyncHandlers (..), serviceHandlers) where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server (ServerStreamHandler, ServiceHandler, UnaryHandler)
import Proto.Utxorpc.V1alpha.Sync.Sync
import Utxorpc.Logged (UtxorpcServiceLogger, loggedSStream, loggedUnary)

data SyncHandlers m a = SyncHandlers
  { fetchBlock :: UnaryHandler m FetchBlockRequest FetchBlockResponse,
    dumpHistory :: UnaryHandler m DumpHistoryRequest DumpHistoryResponse,
    followTip :: ServerStreamHandler m FollowTipRequest FollowTipResponse a,
    readTip :: UnaryHandler m ReadTipRequest ReadTipResponse
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServiceLogger m) ->
  (forall x. m x -> IO x) ->
  SyncHandlers m b ->
  [ServiceHandler]
serviceHandlers logger f SyncHandlers {fetchBlock, dumpHistory, followTip, readTip} =
  [fetchBlockSH, dumpHistorySH, followTipSH, readTipSH]
  where
    fetchBlockSH = loggedUnary f (RPC :: RPC SyncService "fetchBlock") fetchBlock logger
    dumpHistorySH = loggedUnary f (RPC :: RPC SyncService "dumpHistory") dumpHistory logger
    followTipSH = loggedSStream f (RPC :: RPC SyncService "followTip") followTip logger
    readTipSH = loggedUnary f (RPC :: RPC SyncService "readTip") readTip logger
