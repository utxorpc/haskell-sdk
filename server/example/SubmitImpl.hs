module SubmitImpl (handlerImpls) where

import Control.Monad.IO.Class (MonadIO)
import EmptyHandlers (emptySStreamHandler, emptyUnaryHandler)
import Network.GRPC.Server (ServerStreamHandler, UnaryHandler)
import Proto.Utxorpc.V1.Submit.Submit
import Utxorpc.Server (SubmitHandlers (..))

handlerImpls :: (MonadIO m) => (String -> m ()) -> SubmitHandlers m Int Int
handlerImpls logF =
  SubmitHandlers
    (submitTxHandler logF)
    (readMempoolHandler logF)
    (waitForTxHandler logF)
    (watchMempoolHandler logF)

submitTxHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m SubmitTxRequest SubmitTxResponse
submitTxHandler = emptyUnaryHandler

readMempoolHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m ReadMempoolRequest ReadMempoolResponse
readMempoolHandler = emptyUnaryHandler

waitForTxHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  ServerStreamHandler m WaitForTxRequest WaitForTxResponse Int
waitForTxHandler = emptySStreamHandler

watchMempoolHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  ServerStreamHandler m WatchMempoolRequest WatchMempoolResponse Int
watchMempoolHandler = emptySStreamHandler
