module WatchImpl (handlerImpls) where

import Control.Monad.IO.Class (MonadIO)
import EmptyHandlers (emptySStreamHandler)
import Network.GRPC.Server (ServerStreamHandler)
import Proto.Utxorpc.V1.Watch.Watch (WatchTxRequest, WatchTxResponse)
import Utxorpc.Server (WatchHandlers (..))

handlerImpls :: (MonadIO m) => (String -> m ()) -> WatchHandlers m Int
handlerImpls logF =
  WatchHandlers
    (watchTxHandler logF)

watchTxHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  ServerStreamHandler m WatchTxRequest WatchTxResponse Int
watchTxHandler = emptySStreamHandler
