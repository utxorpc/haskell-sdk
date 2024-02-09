{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Logged
  ( UtxorpcServerLogger (..),
    RequestLogger,
    ReplyLogger,
    ServerStreamLogger,
    ServerStreamEndLogger,
    loggedUnary,
    loggedUnaryHandler,
    loggedSStream,
    loggedSStreamHandler,
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

-- | A collection of logging functions that runs in the same monad as
-- the request handlers. Monadic state is passed along throughout the
-- lifecycle of responding to a request. This means that changes to the
-- monadic state in the request logger is seen by the stream logger, stream
-- handler and logger, and reply logger. An `unlift` function to run the monad
-- in IO is provided to @'runUtxorpc'@.
data UtxorpcServerLogger m = UtxorpcServerLogger
  { requestLogger :: RequestLogger m,
    replyLogger :: ReplyLogger m,
    serverStreamLogger :: ServerStreamLogger m,
    serverStreamEndLogger :: ServerStreamEndLogger m
  }

-- | Log requests
type RequestLogger m =
  forall i.
  (Show i) =>
  -- | The RPC path
  BS.ByteString ->
  -- | Request metadata
  Request ->
  -- | A UUID generated for this request and passed to stream and reply loggers.
  UUID ->
  -- | The request message
  i ->
  m ()

-- | Log replies
type ReplyLogger m =
  forall o.
  (Show o) =>
  -- | The RPC path
  BS.ByteString ->
  -- | Request metadata
  Request ->
  -- Generated for the request that caused this reply
  UUID ->
  -- | The reply message
  o ->
  m ()

-- | Log server stream messages
type ServerStreamLogger m =
  forall o.
  (Show o) =>
  -- | The RPC path
  BS.ByteString ->
  -- | Request metadata
  Request ->
  -- | The UUID generated for the request that generated this stream,
  -- and the 0-based index of the message in the stream.
  (UUID, Int) ->
  -- | The stream message
  o ->
  m ()

-- | Log the end of a server stream
type ServerStreamEndLogger m =
  -- | The RPC path
  BS.ByteString ->
  -- | Request metadata
  Request ->
  -- | The UUID generated for the request that generated this stream,
  -- and the 0-based index of the message in the stream.
  (UUID, Int) ->
  m ()

-- | Creates a ServiceHandler that warp-grpc uses to handle requests
loggedUnary ::
  (MonadIO m, GRPCInput r i, GRPCOutput r o, Show i, Show o) =>
  -- | An `unlift` function for the logger and handler monad
  -- Monad state is carried through from request logger, to handler, to reply logger,
  -- So changes to the monad state in the request logger is seen by the handler and reply logger.
  (forall x. m x -> IO x) ->
  -- | The RPC
  r ->
  -- | Generate a reply from request metadata and a proto Message
  UnaryHandler m i o ->
  -- | A logger that runs in the same monad as the handlers
  Maybe (UtxorpcServerLogger m) ->
  ServiceHandler
loggedUnary unlift rpc handler maybeLogger =
  unary unlift rpc $ maybe handler loggedHandler maybeLogger
  where
    -- Generate UUID here for easier testing of `loggedUnaryHandler`
    loggedHandler logger req msg = do
      uuid <- liftIO nextRandom
      loggedUnaryHandler rpc handler uuid logger req msg

loggedUnaryHandler ::
  (MonadIO m, Show i, Show o, IsRPC r) =>
  r ->
  UnaryHandler m i o ->
  UUID ->
  UtxorpcServerLogger m ->
  UnaryHandler m i o
loggedUnaryHandler
  rpc
  handler
  uuid
  UtxorpcServerLogger {requestLogger, replyLogger}
  req
  msg =
    do
      requestLogger (path rpc) req uuid msg
      reply <- handler req msg
      replyLogger rpcPath req uuid reply
      return reply
    where
      rpcPath = path rpc

-- | Creates a ServiceHandler that warp-grpc uses to handle stream requests
loggedSStream ::
  (MonadIO m, GRPCInput r i, GRPCOutput r o, Show i, Show o) =>
  -- | An unlift function for the logger and handler
  -- Monadic state changes are passed from request logger to stream logger and handlers, and so on.
  -- So changes to the monadic state in the request logger are seen by the handler and other loggers.
  (forall x. m x -> IO x) ->
  -- | The RPC
  r ->
  -- | A function that, given request metadata and a protobuf Message,
  -- generates an initial stream state and a function that folds over the stream state to produce
  -- a stream of messages.
  ServerStreamHandler m i o a ->
  -- | A logger that runs in the same monad as the handler
  Maybe (UtxorpcServerLogger m) ->
  ServiceHandler
loggedSStream unlift rpc handler Nothing = serverStream unlift rpc handler
loggedSStream unlift rpc handler (Just logger) =
  serverStream unlift rpc loggedHandler
  where
    loggedHandler req msg = do
      uuid <- liftIO nextRandom
      loggedSStreamHandler rpc handler uuid logger req msg

loggedSStreamHandler ::
  (MonadIO m, IsRPC r, Show i, Show o) =>
  r ->
  ServerStreamHandler m i o a ->
  UUID ->
  UtxorpcServerLogger m ->
  ServerStreamHandler m i o (a, UUID, Int)
loggedSStreamHandler
  rpc
  handler
  uuid
  UtxorpcServerLogger {requestLogger, serverStreamLogger, serverStreamEndLogger}
  req
  msg = do
    -- Log request
    requestLogger rpcPath req uuid msg
    -- Get initial stream state and stream output function
    (initStreamState, ServerStream {serverStreamNext}) <- handler req msg
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
          Just (nextStreamState, replyMsg) -> do
            -- Log chunk
            serverStreamLogger rpcPath req (uuid, index) replyMsg
            -- The unwrapped stream output function returns the next stream state and the message to send
            -- We add log state
            return $ Just ((nextStreamState, uuid, index + 1), replyMsg)

      rpcPath = path rpc
