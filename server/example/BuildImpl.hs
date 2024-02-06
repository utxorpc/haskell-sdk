module BuildImpl (handlerImpls) where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.Server (ServerStreamHandler, UnaryHandler)
import NullHandlers (nullSStreamHandler, nullUnaryHandler)
import Proto.Utxorpc.V1.Build.Build
import Utxorpc.Server (BuildHandlers (..))

handlerImpls :: (MonadIO m) => BuildHandlers m Int
handlerImpls =
  BuildHandlers
    chainTipHandler
    chainParamHandler
    utxoByAddressHandler
    utxoByRefHandler
    holdUtxoHandler

chainTipHandler :: (MonadIO m) => UnaryHandler m GetChainTipRequest GetChainTipResponse
chainTipHandler = nullUnaryHandler

chainParamHandler :: (MonadIO m) => UnaryHandler m GetChainParamRequest GetChainParamResponse
chainParamHandler = nullUnaryHandler

utxoByAddressHandler :: (MonadIO m) => UnaryHandler m GetUtxoByAddressRequest GetUtxoByAddressResponse
utxoByAddressHandler = nullUnaryHandler

utxoByRefHandler :: (MonadIO m) => UnaryHandler m GetUtxoByRefRequest GetUtxoByRefResponse
utxoByRefHandler = nullUnaryHandler

holdUtxoHandler :: (MonadIO m) => ServerStreamHandler m HoldUtxoRequest HoldUtxoResponse Int
holdUtxoHandler = nullSStreamHandler
