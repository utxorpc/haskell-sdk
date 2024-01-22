{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Build where

import Control.Monad.IO.Class (MonadIO)
import Logging (UtxorpcServerLogger, loggedSStream, loggedUnary)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server (ServerStreamHandler, ServiceHandler, UnaryHandler)
import Proto.Utxorpc.Build.V1.Build

data BuildHandlers m a = BuildHandlers
  { getChainTip :: UnaryHandler m GetChainTipRequest GetChainTipResponse,
    getChainParam :: UnaryHandler m GetChainParamRequest GetChainParamResponse,
    getUtxoByAddress :: UnaryHandler m GetUtxoByAddressRequest GetUtxoByAddressResponse,
    getUtxoByRef :: UnaryHandler m GetUtxoByRefRequest GetUtxoByRefResponse,
    holdUtxo :: ServerStreamHandler m HoldUtxoRequest HoldUtxoResponse a
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServerLogger m a) ->
  (forall x. m x -> IO x) ->
  BuildHandlers m b ->
  [ServiceHandler]
serviceHandlers logger f BuildHandlers {getChainTip, getChainParam, getUtxoByAddress, getUtxoByRef, holdUtxo} =
  [chainTipSH, chainParamSH, byAddressSH, byRefSH, holdSH]
  where
    chainTipSH = loggedUnary logger f (RPC :: RPC LedgerStateService "getChainTip") getChainTip
    chainParamSH = loggedUnary logger f (RPC :: RPC LedgerStateService "getChainParam") getChainParam
    byAddressSH = loggedUnary logger f (RPC :: RPC LedgerStateService "getUtxoByAddress") getUtxoByAddress
    byRefSH = loggedUnary logger f (RPC :: RPC LedgerStateService "getUtxoByRef") getUtxoByRef
    holdSH = loggedSStream logger f (RPC :: RPC LedgerStateService "holdUtxo") holdUtxo
