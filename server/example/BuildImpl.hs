module BuildImpl (handlerImpls) where

import Control.Monad.IO.Class (MonadIO)
import EmptyHandlers (emptySStreamHandler, emptyUnaryHandler)
import Network.GRPC.Server (ServerStreamHandler, UnaryHandler)
import Proto.Utxorpc.V1alpha.Build.Build
import Utxorpc.Server (BuildHandlers (..))

handlerImpls ::
  (MonadIO m) =>
  (String -> m ()) ->
  BuildHandlers m Int
handlerImpls logF =
  BuildHandlers
    (chainTipHandler logF)
    (chainParamHandler logF)
    (utxoByAddressHandler logF)
    (utxoByRefHandler logF)
    (holdUtxoHandler logF)

chainTipHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m GetChainTipRequest GetChainTipResponse
chainTipHandler = emptyUnaryHandler

chainParamHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m GetChainParamRequest GetChainParamResponse
chainParamHandler = emptyUnaryHandler

utxoByAddressHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m GetUtxoByAddressRequest GetUtxoByAddressResponse
utxoByAddressHandler = emptyUnaryHandler

utxoByRefHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  UnaryHandler m GetUtxoByRefRequest GetUtxoByRefResponse
utxoByRefHandler = emptyUnaryHandler

holdUtxoHandler ::
  (MonadIO m) =>
  (String -> m ()) ->
  ServerStreamHandler m HoldUtxoRequest HoldUtxoResponse Int
holdUtxoHandler = emptySStreamHandler
