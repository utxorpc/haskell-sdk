{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Sync (SyncHandlers (..), serviceHandlers) where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server (ServerStreamHandler, ServiceHandler, UnaryHandler)
import Proto.Utxorpc.V1.Sync.Sync
import Utxorpc.Logged (UtxorpcServerLogger, loggedSStream, loggedUnary)

data SyncHandlers m a = SyncHandlers
  { fetchBlock :: UnaryHandler m FetchBlockRequest FetchBlockResponse,
    dumpHistory :: UnaryHandler m DumpHistoryRequest DumpHistoryResponse,
    followTip :: ServerStreamHandler m FollowTipRequest FollowTipResponse a
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServerLogger m) ->
  (forall x. m x -> IO x) ->
  SyncHandlers m b ->
  [ServiceHandler]
serviceHandlers logger f SyncHandlers {fetchBlock, dumpHistory, followTip} =
  [fetchBlockSH, dumpHistorySH, followTipSH]
  where
    fetchBlockSH = loggedUnary logger f (RPC :: RPC ChainSyncService "fetchBlock") fetchBlock
    dumpHistorySH = loggedUnary logger f (RPC :: RPC ChainSyncService "dumpHistory") dumpHistory
    followTipSH = loggedSStream logger f (RPC :: RPC ChainSyncService "followTip") followTip
