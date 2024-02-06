{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Logged
  ( UtxorpcServerLogger (..),
    RequestLogger,
    ReplyLogger,
    ServerStreamLogger,
    ServerStreamEndLogger,
    loggedUnary,
    loggedSStream,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Network.GRPC.HTTP2.Encoding (GRPCInput, GRPCOutput)
import Network.GRPC.HTTP2.Types (IsRPC (..))
import Network.GRPC.Server (ServiceHandler, UnaryHandler)
import Network.GRPC.Server.Handlers.Trans (ServerStream (..), ServerStreamHandler, serverStream, unary)
import Network.Wai (Request (..))

data UtxorpcServerLogger m = UtxorpcServerLogger
  { requestLogger :: RequestLogger m,
    replyLogger :: ReplyLogger m,
    serverStreamLogger :: ServerStreamLogger m,
    serverStreamEndLogger :: ServerStreamEndLogger m
  }

type RequestLogger m =
  forall i.
  (Show i) =>
  BS.ByteString ->
  Request ->
  UUID ->
  i ->
  m ()

type ReplyLogger m =
  forall o.
  (Show o) =>
  BS.ByteString ->
  Request ->
  UUID ->
  o ->
  m ()

type ServerStreamLogger m =
  forall o.
  (Show o) =>
  BS.ByteString ->
  Request ->
  (UUID, Int) ->
  o ->
  m ()

type ServerStreamEndLogger m =
  BS.ByteString ->
  Request ->
  (UUID, Int) ->
  m ()

loggedUnary ::
  (MonadIO m, GRPCInput r i, GRPCOutput r o, Show i, Show o) =>
  Maybe (UtxorpcServerLogger m) ->
  (forall x. m x -> IO x) ->
  r ->
  UnaryHandler m i o ->
  ServiceHandler
loggedUnary
  logger
  unlift
  rpc
  handler =
    unary unlift rpc $ maybe handler loggedHandler logger
    where
      loggedHandler UtxorpcServerLogger {requestLogger, replyLogger} req i = do
        uuid <- liftIO nextRandom
        requestLogger (path rpc) req uuid i
        reply <- handler req i
        replyLogger rpcPath req uuid reply
        return reply
      rpcPath = path rpc

loggedSStream ::
  (MonadIO m, GRPCInput r i, GRPCOutput r o, Show i, Show o) =>
  Maybe (UtxorpcServerLogger m) ->
  (forall x. m x -> IO x) ->
  r ->
  ServerStreamHandler m i o a ->
  --  ^ Request -> i -> m (a, ServerStream m o a)
  --      ServerStream m o a ~ ServerStream { serverStreamNext :: a -> m (Maybe (a, o)) }
  ServiceHandler
loggedSStream Nothing unlift rpc handler = serverStream unlift rpc handler
loggedSStream (Just logger) unlift rpc handler =
  serverStream unlift rpc $ loggedHandler logger
  where
    loggedHandler
      UtxorpcServerLogger {requestLogger, serverStreamLogger, serverStreamEndLogger}
      req
      i = do
        uuid <- liftIO nextRandom
        -- Log request
        requestLogger rpcPath req uuid i
        -- Get initial stream state and stream output function
        (initStreamState, ServerStream {serverStreamNext}) <- handler req i
        -- Wrap stream output function with logging
        let loggedStreamNext = mkLoggedStreamNext serverStreamNext
        -- The unwrapped handler returns the initial stream state and stream output function
        -- We add initial log state and return the wrapped stream output function
        return ((initStreamState, uuid, 0), ServerStream loggedStreamNext)
        where
          mkLoggedStreamNext nextChunk (streamState, uuid, index) = do
            -- Get next chunk
            next <- nextChunk streamState
            case next of
              Nothing -> do
                -- Log end of stream
                serverStreamEndLogger rpcPath req (uuid, index)
                -- Return end of stream
                return Nothing
              Just (nextStreamState, msg) -> do
                -- Log chunk
                serverStreamLogger rpcPath req (uuid, index + 1) msg
                -- The unwrapped stream output function returns the next stream state and the message to send
                -- We add log state
                return $ Just ((nextStreamState, uuid, index + 1), msg)
    rpcPath = path rpc
