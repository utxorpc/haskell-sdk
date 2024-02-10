{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Build (BuildHandlers (..), serviceHandlers) where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server (ServerStreamHandler, ServiceHandler, UnaryHandler)
import Proto.Utxorpc.V1.Build.Build
import Utxorpc.Logged (UtxorpcServerLogger, loggedSStream, loggedUnary)

data BuildHandlers m a = BuildHandlers
  { getChainTip :: UnaryHandler m GetChainTipRequest GetChainTipResponse,
    getChainParam :: UnaryHandler m GetChainParamRequest GetChainParamResponse,
    getUtxoByAddress :: UnaryHandler m GetUtxoByAddressRequest GetUtxoByAddressResponse,
    getUtxoByRef :: UnaryHandler m GetUtxoByRefRequest GetUtxoByRefResponse,
    holdUtxo :: ServerStreamHandler m HoldUtxoRequest HoldUtxoResponse a
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServerLogger m) ->
  (forall x. m x -> IO x) ->
  BuildHandlers m b ->
  [ServiceHandler]
serviceHandlers logger f BuildHandlers {getChainTip, getChainParam, getUtxoByAddress, getUtxoByRef, holdUtxo} =
  [chainTipSH, chainParamSH, byAddressSH, byRefSH, holdSH]
  where
    chainTipSH = loggedUnary f (RPC :: RPC LedgerStateService "getChainTip") getChainTip logger
    chainParamSH = loggedUnary f (RPC :: RPC LedgerStateService "getChainParam") getChainParam logger
    byAddressSH = loggedUnary f (RPC :: RPC LedgerStateService "getUtxoByAddress") getUtxoByAddress logger
    byRefSH = loggedUnary f (RPC :: RPC LedgerStateService "getUtxoByRef") getUtxoByRef logger
    holdSH = loggedSStream f (RPC :: RPC LedgerStateService "holdUtxo") holdUtxo logger
