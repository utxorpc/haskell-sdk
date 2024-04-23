module QueryImpl (handlerImpls) where

import Control.Monad.IO.Class (MonadIO)
import EmptyHandlers (emptySStreamHandler, emptyUnaryHandler)
import Network.GRPC.Server (ServerStreamHandler, UnaryHandler)
import Proto.Utxorpc.V1alpha.Query.Query
import Utxorpc.Server (QueryHandlers (..))

handlerImpls ::
  (MonadIO m) =>
  (String -> m ()) ->
  QueryHandlers m Int
handlerImpls logF =
  QueryHandlers
    (readParamsHandler logF)
    (readUtxosHandler logF)
    (searchUtxosHandler logF)
    (streamUtxosHandler logF)

readParamsHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m ReadParamsRequest ReadParamsResponse
readParamsHandler = emptyUnaryHandler

readUtxosHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m ReadUtxosRequest ReadUtxosResponse
readUtxosHandler = emptyUnaryHandler

searchUtxosHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m SearchUtxosRequest SearchUtxosResponse
searchUtxosHandler = emptyUnaryHandler

streamUtxosHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  ServerStreamHandler m ReadUtxosRequest ReadUtxosResponse Int
streamUtxosHandler = emptySStreamHandler
