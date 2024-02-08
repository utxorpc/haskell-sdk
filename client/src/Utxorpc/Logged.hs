{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Logged
  ( UtxorpcClientLogger (..),
    RequestLogger,
    ReplyLogger,
    ServerStreamLogger,
    ServerStreamEndLogger,
    loggedSStream,
    loggedUnary,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (Message)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Network.GRPC.Client (HeaderList, RawReply)
import Network.GRPC.Client.Helpers (GrpcClient (..), rawStreamServer, rawUnary)
import Network.GRPC.HTTP2.Encoding (GRPCInput, GRPCOutput)
import Network.GRPC.HTTP2.Types (IsRPC (..))
import Network.HTTP2.Client.Exceptions (ClientIO)
import Utxorpc.Types (ServerStreamReply, UnaryReply)
import "http2-client" Network.HTTP2.Client (ClientError, TooMuchConcurrency, runClientIO)

{--------------------------------------
  Types
--------------------------------------}

-- | Logging functions to log requests, replies, server stream messages, and server stream endings.
-- A UUID is generated for each request and passed downstream to the other logging functions.
-- `unlift` is a convenience to allow logging functions to be written in any monadic stack by providing the unlift function once, here, instead of at each function definition.
data UtxorpcClientLogger m = UtxorpcClientLogger
  { requestLogger :: RequestLogger m,
    replyLogger :: ReplyLogger m,
    serverStreamLogger :: ServerStreamLogger m,
    serverStreamEndLogger :: ServerStreamEndLogger m,
    unlift :: forall x. m x -> IO x
  }

-- | The ByteString is the method path.
-- The GrcpClient is included because it contains useful information such as the server address
-- A UUID is generated for each call. When a reply or server stream message associated with this call is received, the same UUID is passed to the relevant logger function.
type RequestLogger m =
  forall i.
  (Show i, Message i) =>
  BS.ByteString ->
  GrpcClient ->
  UUID ->
  i -> -- Message sent to the service
  m ()

type ReplyLogger m =
  forall o.
  (Show o, Message o) =>
  -- | The ByteString is the method path.
  BS.ByteString ->
  -- | Included because it contains useful information such as the server address.
  GrpcClient ->
  -- | Generated for the request that this reply is associated with.
  UUID ->
  -- | Message received from the service (with headers) or an error.
  Either ClientError (Either TooMuchConcurrency (RawReply o)) ->
  m ()

-- | A function to log server stream messages.
type ServerStreamLogger m =
  forall o.
  (Show o, Message o) =>
  -- | The ByteString is the method path.
  BS.ByteString ->
  -- | Included because it contains useful information such as the server address.
  GrpcClient ->
  -- | The UUID was generated for the request that caused this reply, the Int is the index of this message in the stream.
  (UUID, Int) ->
  -- | Message received from the service.
  o ->
  m ()

-- The GrcpClient is included because it contains useful information such as the server address.
-- The UUID was generated for the request that this reply is associated with.
type ServerStreamEndLogger m =
  -- | The ByteString is the method path.
  BS.ByteString ->
  -- | Included because it contains useful information such as the server address.
  GrpcClient ->
  -- | The UUID was generated for the request that caused this reply, the Int is the total number of messages received in the stream.
  (UUID, Int) ->
  -- | Headers and Trailers.
  (HeaderList, HeaderList) ->
  m ()

-- The type of http-client-grpc's `rawUnary`. Used internally.
type UnaryExecutor r i o =
  -- | The RPC to call.
  r ->
  -- | An initialized client.
  GrpcClient ->
  -- | The input.
  i ->
  ClientIO (Either TooMuchConcurrency (RawReply o))

-- The type of http2-client-grpc's `rawStreamServer`. Used internally.
type ServerStreamExecutor r i o =
  forall a.
  -- | The RPC to call.
  r ->
  -- | An initialized client.
  GrpcClient ->
  -- | An initial state.
  a ->
  -- | The input of the stream request.
  i ->
  -- | A state-passing handler called for each server-sent output.
  -- Headers are repeated for convenience but are the same for every iteration.
  (a -> HeaderList -> o -> ClientIO a) ->
  ClientIO (Either TooMuchConcurrency (a, HeaderList, HeaderList))

{--------------------------------------
  Logged wrappers of gRPC library functions
--------------------------------------}

loggedUnary ::
  (GRPCInput r i, GRPCOutput r o, Show i, Message i, Show o, Message o) =>
  Maybe (UtxorpcClientLogger m) ->
  r ->
  GrpcClient ->
  i ->
  UnaryReply o
loggedUnary = loggedUnary' rawUnary

loggedUnary' ::
  (GRPCInput r i, GRPCOutput r o, Show i, Message i, Show o, Message o) =>
  UnaryExecutor r i o ->
  Maybe (UtxorpcClientLogger m) ->
  r ->
  GrpcClient ->
  i ->
  UnaryReply o
loggedUnary' sendUnary logger rpc client msg =
  maybe (runClientIO $ sendUnary rpc client msg) logged logger
  where
    logged UtxorpcClientLogger {requestLogger, replyLogger, unlift} = do
      uuid <- nextRandom
      unlift $ requestLogger (path rpc) client uuid msg
      o <- runClientIO $ sendUnary rpc client msg
      unlift $ replyLogger (path rpc) client uuid o
      return o

-- The gRPC library requires a handler that produces a `ClientIO`,
-- but this does not make sense since a user-provided handler is not
-- likely to generate a `ClientError` or `TooMuchConcurrency`.
-- Instead, we accept a handler of type `IO` and lift it.
loggedSStream ::
  (GRPCOutput r o, GRPCInput r i, Show i, Message i, Show o, Message o) =>
  Maybe (UtxorpcClientLogger m) ->
  r ->
  GrpcClient ->
  a ->
  i ->
  (a -> HeaderList -> o -> IO a) ->
  ServerStreamReply a
loggedSStream = loggedSStream' rawStreamServer

loggedSStream' ::
  (GRPCOutput r o, GRPCInput r i, Show i, Message i, Show o, Message o) =>
  ServerStreamExecutor r i o ->
  Maybe (UtxorpcClientLogger m) ->
  r ->
  GrpcClient ->
  a ->
  i ->
  (a -> HeaderList -> o -> IO a) ->
  ServerStreamReply a
loggedSStream' sendStreamReq logger r client initStreamState req chunkHandler =
  maybe
    (runClientIO $ sendStreamReq r client initStreamState req liftedChunkHandler)
    logged
    logger
  where
    liftedChunkHandler streamState headerList reply = liftIO $ chunkHandler streamState headerList reply

    logged
      UtxorpcClientLogger {requestLogger, serverStreamLogger, serverStreamEndLogger, unlift} = do
        uuid <- nextRandom
        unlift $ requestLogger rpcPath client uuid req
        streamResult <- runLoggedStream uuid
        handleStreamResult streamResult
        where
          runLoggedStream uuid =
            runClientIO $
              sendStreamReq r client (initStreamState, uuid, 0) req loggedChunkHandler

          loggedChunkHandler (streamState, uuid, index) headerList msg = do
            liftIO . unlift $ serverStreamLogger rpcPath client (uuid, index) msg
            a <- liftIO $ chunkHandler streamState headerList msg
            return (a, uuid, index + 1)

          handleStreamResult streamResult =
            case streamResult of
              Right (Right ((finalStreamState, uuid, index), hl, hl')) -> do
                liftIO . unlift $ serverStreamEndLogger rpcPath client (uuid, index) (hl, hl')
                return $ Right $ Right (finalStreamState, hl, hl')
              Right (Left tmc) -> return $ Right $ Left tmc
              Left errCode -> return $ Left errCode

          rpcPath = path r
