{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Submit where

import Control.Monad.IO.Class (MonadIO)
import Logging (UtxorpcServerLogger, loggedSStream, loggedUnary)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server (ServerStreamHandler, ServiceHandler, UnaryHandler)
import Proto.Utxorpc.Submit.V1.Submit

data SubmitHandlers m a b = SubmitHandlers
  { submitTx :: UnaryHandler m SubmitTxRequest SubmitTxResponse,
    readMempool :: UnaryHandler m ReadMempoolRequest ReadMempoolResponse,
    waitForTx :: ServerStreamHandler m WaitForTxRequest WaitForTxResponse a,
    watchMempool :: ServerStreamHandler m WatchMempoolRequest WatchMempoolResponse b
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServerLogger m a) ->
  (forall x. m x -> IO x) ->
  SubmitHandlers m b c ->
  [ServiceHandler]
serviceHandlers logger f SubmitHandlers {submitTx, readMempool, waitForTx, watchMempool} =
  [submitTxSH, readMempoolSH, waitForTxSH, watchMempoolSH]
  where
    submitTxSH = loggedUnary logger f (RPC :: RPC SubmitService "submitTx") submitTx
    readMempoolSH = loggedUnary logger f (RPC :: RPC SubmitService "readMempool") readMempool
    waitForTxSH = loggedSStream logger f (RPC :: RPC SubmitService "waitForTx") waitForTx
    watchMempoolSH = loggedSStream logger f (RPC :: RPC SubmitService "watchMempool") watchMempool
