module SyncImpl (handlerImpls) where

import Control.Monad.IO.Class (MonadIO)
import EmptyHandlers (emptySStreamHandler, emptyUnaryHandler)
import Network.GRPC.Server (ServerStreamHandler, UnaryHandler)
import Proto.Utxorpc.V1.Sync.Sync (DumpHistoryRequest, DumpHistoryResponse, FetchBlockRequest, FetchBlockResponse, FollowTipRequest, FollowTipResponse)
import Utxorpc.Server (SyncHandlers (..))

handlerImpls ::
  (MonadIO m) =>
  (String -> m ()) ->
  SyncHandlers m Int
handlerImpls logF =
  SyncHandlers
    (fetchBlockHandler logF)
    (dumpHistoryHandler logF)
    (followTipHandler logF)

fetchBlockHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m FetchBlockRequest FetchBlockResponse
fetchBlockHandler = emptyUnaryHandler

dumpHistoryHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m DumpHistoryRequest DumpHistoryResponse
dumpHistoryHandler = emptyUnaryHandler

followTipHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  ServerStreamHandler m FollowTipRequest FollowTipResponse Int
followTipHandler = emptySStreamHandler
