module SyncImpl (handlerImpls) where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.Server (ServerStreamHandler, UnaryHandler)
import NullHandlers (nullSStreamHandler, nullUnaryHandler)
import Proto.Utxorpc.V1.Sync.Sync (DumpHistoryRequest, DumpHistoryResponse, FetchBlockRequest, FetchBlockResponse, FollowTipRequest, FollowTipResponse)
import Utxorpc.Server (SyncHandlers (..))

handlerImpls :: (MonadIO m) => SyncHandlers m Int
handlerImpls =
  SyncHandlers
    fetchBlockHandler
    dumpHistoryHandler
    followTipHandler

fetchBlockHandler :: (MonadIO m) => UnaryHandler m FetchBlockRequest FetchBlockResponse
fetchBlockHandler = nullUnaryHandler

dumpHistoryHandler :: (MonadIO m) => UnaryHandler m DumpHistoryRequest DumpHistoryResponse
dumpHistoryHandler = nullUnaryHandler

followTipHandler :: (MonadIO m) => ServerStreamHandler m FollowTipRequest FollowTipResponse Int
followTipHandler = nullSStreamHandler
