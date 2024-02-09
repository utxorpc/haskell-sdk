{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Submit (SubmitHandlers (..), serviceHandlers) where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server (ServerStreamHandler, ServiceHandler, UnaryHandler)
import Proto.Utxorpc.V1.Submit.Submit
import Utxorpc.Logged (UtxorpcServerLogger, loggedSStream, loggedUnary)

data SubmitHandlers m a b = SubmitHandlers
  { submitTx :: UnaryHandler m SubmitTxRequest SubmitTxResponse,
    readMempool :: UnaryHandler m ReadMempoolRequest ReadMempoolResponse,
    waitForTx :: ServerStreamHandler m WaitForTxRequest WaitForTxResponse a,
    watchMempool :: ServerStreamHandler m WatchMempoolRequest WatchMempoolResponse b
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServerLogger m) ->
  (forall x. m x -> IO x) ->
  SubmitHandlers m b c ->
  [ServiceHandler]
serviceHandlers logger f SubmitHandlers {submitTx, readMempool, waitForTx, watchMempool} =
  [submitTxSH, readMempoolSH, waitForTxSH, watchMempoolSH]
  where
    submitTxSH = loggedUnary f (RPC :: RPC SubmitService "submitTx") submitTx logger
    readMempoolSH = loggedUnary f (RPC :: RPC SubmitService "readMempool") readMempool logger
    waitForTxSH = loggedSStream f (RPC :: RPC SubmitService "waitForTx") waitForTx logger
    watchMempoolSH = loggedSStream f (RPC :: RPC SubmitService "watchMempool") watchMempool logger
