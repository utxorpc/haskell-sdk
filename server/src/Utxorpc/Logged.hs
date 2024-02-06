{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Logged
  ( UtxorpcServerLogger (..),
    RequestLogger,
    ReplyLogger,
    ServerChunkLogger,
    ServerStreamEndLogger,
    loggedUnary,
    loggedSStream,
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (Message)
import Network.GRPC.HTTP2.Encoding (GRPCInput, GRPCOutput)
import Network.GRPC.HTTP2.Types (IsRPC (..))
import Network.GRPC.Server (ServiceHandler, UnaryHandler)
import Network.GRPC.Server.Handlers.Trans (ServerStream (..), ServerStreamHandler, serverStream, unary)
import Network.Wai (Request (..))

data UtxorpcServerLogger m a = UtxorpcServerLogger
  { requestL :: RequestLogger m a,
    replyL :: ReplyLogger m a,
    serverChunkL :: ServerChunkLogger m a,
    serverStreamEndL :: ServerStreamEndLogger m a
  }

type RequestLogger m a =
  forall i.
  (Message i, Show i) =>
  BS.ByteString ->
  Request ->
  i ->
  m a

type ReplyLogger m a =
  forall o.
  (Message o, Show o) =>
  BS.ByteString ->
  Request ->
  a ->
  o ->
  m ()

type ServerChunkLogger m a =
  forall o.
  (Message o, Show o) =>
  BS.ByteString ->
  Request ->
  a ->
  o ->
  m a

type ServerStreamEndLogger m a = BS.ByteString -> Request -> a -> m ()

loggedUnary ::
  (MonadIO m, GRPCInput r i, GRPCOutput r o, Message i, Show i, Message o, Show o) =>
  Maybe (UtxorpcServerLogger m a) ->
  (forall x. m x -> IO x) ->
  r ->
  UnaryHandler m i o ->
  ServiceHandler
loggedUnary logger f rpc handler =
  unary f rpc $ maybe handler loggedHandler logger
  where
    loggedHandler l req i = do
      initLogState <- requestL l (path rpc) req i
      reply <- handler req i
      replyL l rpcPath req initLogState reply
      return reply
    rpcPath = path rpc

loggedSStream ::
  (MonadIO m, GRPCInput r i, GRPCOutput r o, Message i, Show i, Message o, Show o) =>
  Maybe (UtxorpcServerLogger m b) ->
  (forall x. m x -> IO x) ->
  r ->
  ServerStreamHandler m i o a ->
  --  ^ Request -> i -> m (a, ServerStream m o a)
  --      ServerStream m o a ~ ServerStream { serverStreamNext :: a -> m (Maybe (a, o)) }
  ServiceHandler
loggedSStream Nothing f rpc handler = serverStream f rpc handler
loggedSStream (Just logger) f rpc handler =
  serverStream f rpc $ loggedHandler logger
  where
    loggedHandler UtxorpcServerLogger {requestL, serverChunkL, serverStreamEndL} req i = do
      initLogState <- requestL rpcPath req i
      -- \^ Log request
      (initStreamState, ServerStream {serverStreamNext}) <- handler req i
      -- \^ Get next stream chunk generator
      let loggedStreamNext = mkLoggedStreamNext serverStreamNext
      -- \^ Wrap next stream chunk generator in logging functions
      return ((initStreamState, initLogState), ServerStream loggedStreamNext)
      where
        mkLoggedStreamNext nextChunk (streamState, logState) = do
          res <- nextChunk streamState
          -- \^ Get next chunk
          case res of
            Nothing -> do
              serverStreamEndL rpcPath req logState
              -- \^ Log end of stream
              return Nothing
            -- \^ Return end of stream
            Just (nextStreamState, msg) -> do
              nextLogState <- serverChunkL rpcPath req logState msg
              -- Log chunk
              return $ Just ((nextStreamState, nextLogState), msg)
    rpcPath = path rpc
