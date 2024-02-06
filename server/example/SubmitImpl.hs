module SubmitImpl (handlerImpls) where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.Server (ServerStreamHandler, UnaryHandler)
import NullHandlers (nullSStreamHandler, nullUnaryHandler)
import Proto.Utxorpc.V1.Submit.Submit
import Utxorpc.Server (SubmitHandlers (..))

handlerImpls :: (MonadIO m) => SubmitHandlers m Int Int
handlerImpls =
  SubmitHandlers
    submitTxHandler
    readMempoolHandler
    waitForTxHandler
    watchMempoolHandler

submitTxHandler :: (MonadIO m) => UnaryHandler m SubmitTxRequest SubmitTxResponse
submitTxHandler = nullUnaryHandler

readMempoolHandler :: (MonadIO m) => UnaryHandler m ReadMempoolRequest ReadMempoolResponse
readMempoolHandler = nullUnaryHandler

waitForTxHandler :: (MonadIO m) => ServerStreamHandler m WaitForTxRequest WaitForTxResponse Int
waitForTxHandler = nullSStreamHandler

watchMempoolHandler :: (MonadIO m) => ServerStreamHandler m WatchMempoolRequest WatchMempoolResponse Int
watchMempoolHandler = nullSStreamHandler
