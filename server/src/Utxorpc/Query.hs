{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Query (QueryHandlers (..), serviceHandlers) where

import Control.Monad.IO.Class (MonadIO)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server (ServerStreamHandler, ServiceHandler, UnaryHandler)
import Proto.Utxorpc.V1alpha.Query.Query
import Utxorpc.Logged (UtxorpcServiceLogger, loggedSStream, loggedUnary)

data QueryHandlers m = QueryHandlers
  { readParams :: UnaryHandler m ReadParamsRequest ReadParamsResponse,
    readUtxos :: UnaryHandler m ReadUtxosRequest ReadUtxosResponse,
    searchUtxos :: UnaryHandler m SearchUtxosRequest SearchUtxosResponse
  }

serviceHandlers ::
  (MonadIO m) =>
  Maybe (UtxorpcServiceLogger m) ->
  (forall x. m x -> IO x) ->
  QueryHandlers m ->
  [ServiceHandler]
serviceHandlers logger f QueryHandlers {readParams, readUtxos, searchUtxos} =
  [readParamsSH, readUtxosSH, searchUtxosSH]
  where
    readParamsSH = loggedUnary f (RPC :: RPC QueryService "readParams") readParams logger
    readUtxosSH = loggedUnary f (RPC :: RPC QueryService "readUtxos") readUtxos logger
    searchUtxosSH = loggedUnary f (RPC :: RPC QueryService "searchUtxos") searchUtxos logger
