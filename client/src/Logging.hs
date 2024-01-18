{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  ( UtxorpcClientLogger,
    RequestLogger,
    ReplyLogger,
    ServerChunkLogger,
    ServerStreamEndLogger,
    loggedUnary,
    loggedSStream,
  )
where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI (original))
import Data.ProtoLens (Message)
import Data.Time (getZonedTime)
import Data.UUID (UUID, nil)
import Data.UUID.V4 (nextRandom)
import Network.GRPC.Client (HeaderList, RawReply)
import Network.GRPC.Client.Helpers (GrpcClient (..), rawStreamServer, rawUnary)
import Network.GRPC.HTTP2.Encoding (GRPCInput, GRPCOutput)
import Network.GRPC.HTTP2.Types (IsRPC (..))
import Network.HTTP2.Client.Exceptions (ClientIO)
import Types (ServerStreamReply, UnaryReply)

data UtxorpcClientLogger m a = UtxorpcClientLogger
  { requestL :: RequestLogger m a,
    replyL :: ReplyLogger m a,
    serverStreamDataL :: ServerChunkLogger m a,
    serverStreamEndL :: ServerStreamEndLogger m a,
    unlift :: forall x. m x -> IO x
  }

type RequestLogger m a =
  forall i.
  (Show i, Message i) =>
  BS.ByteString ->
  GrpcClient ->
  i ->
  m a

type ReplyLogger m a =
  forall o.
  (Show o, Message o) =>
  BS.ByteString ->
  GrpcClient ->
  a ->
  RawReply o ->
  m ()

type ServerChunkLogger m a =
  forall o.
  (Show o, Message o) =>
  BS.ByteString ->
  GrpcClient ->
  a ->
  o ->
  m a

type ServerStreamEndLogger m a =
  BS.ByteString ->
  GrpcClient ->
  (a, HeaderList, HeaderList) ->
  m ()

loggedUnary ::
  (GRPCInput r i, GRPCOutput r o, Show i, Message i, Show o, Message o) =>
  Maybe (UtxorpcClientLogger m a) ->
  r ->
  GrpcClient ->
  i ->
  UnaryReply o
loggedUnary = maybe rawUnary loggedUnary'

loggedUnary' ::
  (GRPCInput r i, GRPCOutput r o, Show i, Message i, Show o, Message o) =>
  UtxorpcClientLogger m a ->
  r ->
  GrpcClient ->
  i ->
  UnaryReply o
loggedUnary' (UtxorpcClientLogger {requestL, replyL, unlift}) r client i = do
  a <- liftIO . unlift $ requestL (path r) client i
  o <- rawUnary r client i
  case o of
    Right rawReply -> liftIO . unlift $ replyL (path r) client a rawReply
    _ -> return () {-- TODO: Need to handle `TooMuchConcurrency` --}
  return o

loggedSStream ::
  (GRPCOutput r o, GRPCInput r i, Show i, Message i, Show o, Message o) =>
  Maybe (UtxorpcClientLogger m c) ->
  r ->
  GrpcClient ->
  a ->
  i ->
  (a -> HeaderList -> o -> ClientIO a) ->
  ServerStreamReply a
loggedSStream = maybe rawStreamServer loggedServerStream'

loggedServerStream' ::
  (GRPCOutput r o, GRPCInput r i, Show i, Message i, Show o, Message o) =>
  UtxorpcClientLogger m c ->
  r ->
  GrpcClient ->
  a ->
  i ->
  (a -> HeaderList -> o -> ClientIO a) ->
  ServerStreamReply a
loggedServerStream'
  (UtxorpcClientLogger {requestL, serverStreamDataL, serverStreamEndL, unlift})
  r
  client
  initStreamState
  req
  chunkHandler = do
    initLogState <- liftIO logRequestIO
    streamResult <- runLoggedStream initLogState
    handleStreamResult streamResult
    where
      logRequestIO = unlift $ requestL rpcPath client req

      runLoggedStream initLogState =
        rawStreamServer r client (initStreamState, initLogState) req loggedChunkHandler

      loggedChunkHandler state hl o = do
        a <- chunkHandler (fst state) hl o
        b <- liftIO . unlift $ serverStreamDataL rpcPath client (snd state) o
        return (a, b)

      handleStreamResult streamResult =
        case streamResult of
          Right ((finalStreamState, finalLogState), hl, hl') -> do
            liftIO $ logEndOfStreamIO finalLogState hl hl'
            return $ Right (finalStreamState, hl, hl')
          Left tmc -> return $ Left tmc

      logEndOfStreamIO finalLogState hl hl' =
        unlift $ serverStreamEndL rpcPath client (finalLogState, hl, hl')

      rpcPath = path r
