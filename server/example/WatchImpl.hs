module WatchImpl (handlerImpls) where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.Server (ServerStreamHandler)
import NullHandlers (nullSStreamHandler)
import Proto.Utxorpc.V1.Watch.Watch (WatchTxRequest, WatchTxResponse)
import Utxorpc.Server (WatchHandlers (..))

handlerImpls :: (MonadIO m) => WatchHandlers m Int
handlerImpls =
  WatchHandlers
    watchTxHandler

watchTxHandler :: (MonadIO m) => ServerStreamHandler m WatchTxRequest WatchTxResponse Int
watchTxHandler = nullSStreamHandler
